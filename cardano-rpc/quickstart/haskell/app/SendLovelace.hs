{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Cardano.Api
import qualified Cardano.Api.Experimental as Exp
import qualified Cardano.Api.Experimental.Tx as Exp
import qualified Cardano.Api.Ledger as L
import Cardano.Rpc.Client (defMessage, (&), (.~), (^.))
import qualified Cardano.Rpc.Client as Rpc
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Query as Query
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Submit as Submit

import Control.Concurrent (threadDelay)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import Data.Default.Class (def)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T (show, strip, unpack)
import qualified Data.Text.Encoding as T (decodeUtf8)
import qualified Data.Text.IO as T (putStrLn, readFile)
import System.Exit (die)

-- | Address of a locally running node's gRPC endpoint, started with
-- @--enable-grpc-http@ (cardano-testnet), as set up in the quickstart README.
rpcAddress :: Rpc.Address
rpcAddress =
  Rpc.Address
    { Rpc.addressHost = "127.0.0.1"
    , Rpc.addressPort = 50_051
    , Rpc.addressAuthority = Nothing
    }

clusterDir :: FilePath
clusterDir = "/tmp/demo-cluster"

lovelaceToSend :: Integer
lovelaceToSend = 5_000_000 -- 5 ADA (1 ADA = 1,000,000 lovelace)

era :: Exp.Era ConwayEra
era = Exp.ConwayEra

main :: IO ()
main = do
  senderWitness <-
    orDie (docToString . prettyError)
      =<< readFileTextEnvelopeAnyOf
        [FromSomeType asType WitnessGenesisUTxOKey]
        (File $ clusterDir <> "/utxo-keys/utxo1/utxo.skey")

  senderAddressText <- T.strip <$> T.readFile (clusterDir <> "/utxo-keys/utxo1/utxo.addr")
  recipientAddressText <- T.strip <$> T.readFile (clusterDir <> "/utxo-keys/utxo2/utxo.addr")
  senderAddress <- parseAddress senderAddressText
  recipientAddress <- parseAddress recipientAddressText

  T.putStrLn $ "Sender address:    " <> senderAddressText
  T.putStrLn $ "Recipient address: " <> recipientAddressText

  Rpc.withConnection def (Rpc.ServerInsecure rpcAddress) $ \conn -> do
    utxoItems <- searchUtxos conn senderAddress
    T.putStrLn $ "Spendable UTxOs:   " <> T.show (length utxoItems)

    (minFeeCoefficient, minFeeConstant) <- readMinFeeParams conn

    let utxoTxIns = map (itemToTxIn . (^. Query.txoRef)) utxoItems
        totalInputLovelace = sum $ map itemCoin utxoItems
        lovelaceAfterSend = totalInputLovelace - lovelaceToSend
        recipientTxOut = mkTxOut recipientAddress lovelaceToSend

    changeLovelace <-
      orDie id $
        if lovelaceAfterSend > 0
          then Right lovelaceAfterSend
          else Left "Sender's UTxOs do not cover 5 ADA plus fees"

    let signedTx =
          buildAndSignTx
            senderWitness
            utxoTxIns
            recipientTxOut
            senderAddress
            changeLovelace
            minFeeCoefficient
            minFeeConstant
        rawTxBytes = serialiseToRawBytes signedTx

    submitResponse <-
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf Submit.SubmitService "submitTx")) $
        defMessage & Submit.tx .~ (defMessage & Submit.raw .~ rawTxBytes)
    let submittedTxHash = submitResponse ^. Submit.ref
    T.putStrLn $ "Submitted tx: " <> hexEncode submittedTxHash

    confirmDelivery conn recipientAddress recipientAddressText submittedTxHash

-- | Build a transaction spending all of @utxoTxIns@ into @recipientTxOut@
-- plus a change output back to the sender, then sign it with the sender's
-- key. The fee is found by an estimate-measure-reestimate loop: since there
-- are no scripts, minimum fee is exactly @txSizeInBytes * minFeeCoefficient +
-- minFeeConstant@, and the only unknown ahead of time is the encoded size of
-- the fee and change amounts themselves, which stabilises after one or two
-- iterations.
buildAndSignTx
  :: ShelleyWitnessSigningKey
  -> [TxIn]
  -> Exp.TxOut (Exp.LedgerEra ConwayEra)
  -> AddressInEra ConwayEra
  -> Integer
  -> Integer
  -> Integer
  -> Exp.SignedTx ConwayEra
buildAndSignTx senderWitness utxoTxIns recipientTxOut senderAddress changeLovelace minFeeCoefficient minFeeConstant =
  go 0
 where
  go :: Integer -> Exp.SignedTx ConwayEra
  go fee =
    let content =
          Exp.defaultTxBodyContent
            & Exp.setTxIns [(txIn, Exp.AnyKeyWitnessPlaceholder) | txIn <- utxoTxIns]
            & Exp.setTxOuts [recipientTxOut, mkTxOut senderAddress (changeLovelace - fee)]
            & Exp.setTxFee (L.Coin fee)
        unsignedTx = either (error . docToString . prettyError) id $ Exp.makeUnsignedTx era content
        keyWitness = Exp.makeKeyWitness era unsignedTx senderWitness
        signedTx = Exp.signTx era [] [keyWitness] unsignedTx
        txSize = toInteger $ BS.length $ serialiseToRawBytes signedTx
        requiredFee = txSize * minFeeCoefficient + minFeeConstant
     in if requiredFee <= fee then signedTx else go requiredFee

mkTxOut :: AddressInEra ConwayEra -> Integer -> Exp.TxOut (Exp.LedgerEra ConwayEra)
mkTxOut address lovelace =
  Exp.TxOut $ L.mkBasicTxOut (toShelleyAddr address) (L.inject $ L.Coin lovelace)

-- | Poll the recipient's UTxOs until one matches the submitted transaction
-- hash, for up to a minute.
confirmDelivery :: Rpc.Connection -> AddressInEra ConwayEra -> Text -> BS.ByteString -> IO ()
confirmDelivery conn recipientAddress recipientAddressText submittedTxHash = go (20 :: Int)
 where
  go attemptsLeft
    | attemptsLeft <= 0 =
        orDie id $ Left "Timed out waiting for the new UTxO to appear at the recipient address"
    | otherwise = do
        utxoItems <- searchUtxos conn recipientAddress
        case listToMaybe [item | item <- utxoItems, item ^. Query.txoRef . Query.hash == submittedTxHash] of
          Just item ->
            T.putStrLn $
              "Confirmed: "
                <> T.show (itemCoin item)
                <> " lovelace landed at "
                <> recipientAddressText
                <> " ("
                <> hexEncode submittedTxHash
                <> "#"
                <> T.show (item ^. Query.txoRef . Query.index)
                <> ")"
          Nothing -> do
            threadDelay 3_000_000
            go (attemptsLeft - 1)

searchUtxos :: Rpc.Connection -> AddressInEra ConwayEra -> IO [Rpc.Proto Query.AnyUtxoData]
searchUtxos conn address = do
  response <-
    Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf Query.QueryService "searchUtxos")) $
      defMessage & Query.predicate .~ exactAddressPredicate address
  pure $ response ^. Query.items

readMinFeeParams :: Rpc.Connection -> IO (Integer, Integer)
readMinFeeParams conn = do
  response <-
    Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf Query.QueryService "readParams")) defMessage
  let params = response ^. Query.values . Query.cardano
  pure
    ( toInteger $ params ^. Query.minFeeCoefficient . Query.int
    , toInteger $ params ^. Query.minFeeConstant . Query.int
    )

itemCoin :: Rpc.Proto Query.AnyUtxoData -> Integer
itemCoin item = toInteger $ item ^. Query.cardano . Query.coin . Query.int

itemToTxIn :: Rpc.Proto Query.TxoRef -> TxIn
itemToTxIn txoRef =
  case deserialiseFromRawBytes AsTxId (txoRef ^. Query.hash) of
    Left err -> error $ "Malformed txo ref hash from cardano-rpc: " <> show err
    Right utxoTxId -> TxIn utxoTxId (TxIx (fromIntegral (txoRef ^. Query.index)))

-- | Match a UTxO exactly by its address, mirroring cardano-rpc's own
-- (internal) 'exactAddressPredicate'.
exactAddressPredicate :: AddressInEra ConwayEra -> Rpc.Proto Query.UtxoPredicate
exactAddressPredicate address =
  defMessage
    & Query.match
      .~ ( defMessage
             & Query.cardano
               .~ (defMessage & Query.address .~ (defMessage & Query.exactAddress .~ serialiseToRawBytes address))
         )

parseAddress :: Text -> IO (AddressInEra ConwayEra)
parseAddress addressText =
  orDie id $
    maybe (Left $ "Could not parse address: " <> T.unpack addressText) Right $
      deserialiseAddress (AsAddressInEra asType) addressText

hexEncode :: BS.ByteString -> Text
hexEncode = T.decodeUtf8 . Base16.encode

orDie :: (e -> String) -> Either e a -> IO a
orDie render = either (die . render) pure
