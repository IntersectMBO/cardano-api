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

import qualified Cardano.Ledger.Coin as L
import qualified Cardano.Ledger.Conway.Core as L
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Time (mkSlotLength)

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import Data.Default.Class (def)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T (show, strip, unpack)
import qualified Data.Text.Encoding as T (decodeUtf8)
import qualified Data.Text.IO as T (putStrLn, readFile)
import Data.Time (UTCTime (..))
import Data.Time.Calendar (fromGregorian)
import GHC.Exts (IsList (..))

import UnliftIO.Exception (stringException, throwString)

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

conwayEra :: Exp.Era ConwayEra
conwayEra = Exp.ConwayEra

main :: IO ()
main = do
  senderWitness <-
    orDie (docToString . prettyError)
      =<< readFileTextEnvelopeAnyOf
        [FromSomeType asType WitnessGenesisUTxOKey]
        (File $ clusterDir <> "/utxo-keys/utxo1/utxo.skey")

  senderAddressText <- T.strip <$> T.readFile (clusterDir <> "/utxo-keys/utxo1/utxo.addr")
  recipientAddressText <- T.strip <$> T.readFile (clusterDir <> "/utxo-keys/utxo2/utxo.addr")
  senderAddress <- parseAddress conwayEra senderAddressText
  recipientAddress <- parseAddress conwayEra recipientAddressText

  T.putStrLn $ "Sender address:    " <> senderAddressText
  T.putStrLn $ "Recipient address: " <> recipientAddressText

  Rpc.withConnection def (Rpc.ServerInsecure rpcAddress) $ \conn -> do
    utxoItems <- searchUtxos conwayEra conn senderAddress
    T.putStrLn $ "Spendable UTxOs:   " <> T.show (length utxoItems)

    pparams <- readProtocolParams conwayEra conn

    utxoTxIns <- traverse (itemToTxIn . (^. Query.txoRef)) utxoItems
    itemCoins <- traverse itemCoin utxoItems
    let totalInputLovelace = sum itemCoins
        recipientTxOut = Exp.TxOut $ mkLedgerTxOut conwayEra recipientAddress lovelaceToSend
        ledgerUtxo =
          L.UTxO $
            fromList
              [ (toShelleyTxIn txIn, mkLedgerTxOut conwayEra senderAddress coin)
              | (txIn, coin) <- zip utxoTxIns itemCoins
              ]

    when (totalInputLovelace <= lovelaceToSend) $
      throwString "Sender's UTxOs do not cover 5 ADA plus fees"

    signedTx <-
      buildAndSignTx conwayEra senderWitness ledgerUtxo utxoTxIns recipientTxOut senderAddress pparams
    let rawTxBytes = serialiseToRawBytes signedTx

    submitResponse <-
      Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf Submit.SubmitService "submitTx")) $
        defMessage & Submit.tx .~ (defMessage & Submit.raw .~ rawTxBytes)
    let submittedTxHash = submitResponse ^. Submit.ref
    T.putStrLn $ "Submitted tx: " <> hexEncode submittedTxHash

    confirmDelivery conwayEra conn recipientAddress recipientAddressText submittedTxHash

-- | Build a transaction spending @utxoTxIns@ into @recipientTxOut@, balanced
-- and signed by cardano-api's 'Exp.makeTransactionBodyAutoBalance': it works
-- out the minimum fee and the change output back to the sender itself, given
-- the ledger 'L.PParams' fetched by 'readProtocolParams'.
buildAndSignTx
  :: MonadThrow m
  => Exp.Era era
  -- ^ The era to build the transaction in.
  -> ShelleyWitnessSigningKey
  -- ^ Signs the transaction; must match the change address below.
  -> L.UTxO (Exp.LedgerEra era)
  -- ^ All inputs being spent, needed to work out their total value for balancing.
  -> [TxIn]
  -- ^ The transaction inputs to spend.
  -> Exp.TxOut (Exp.LedgerEra era)
  -- ^ The payment output to the recipient.
  -> AddressInEra era
  -- ^ Change address; the leftover balance is sent back here.
  -> L.PParams (Exp.LedgerEra era)
  -- ^ Protocol parameters; only the fields 'readProtocolParams' maps are read.
  -> m (Exp.SignedTx era)
buildAndSignTx era senderWitness ledgerUtxo utxoTxIns recipientTxOut senderAddress pparams = do
  let content =
        Exp.defaultTxBodyContent
          & Exp.setTxIns [(txIn, Exp.AnyKeyWitnessPlaceholder) | txIn <- utxoTxIns]
          & Exp.setTxOuts [recipientTxOut]

  (unsignedTx, _balancedContent) <-
    either (throwM . stringException . docToString . prettyError) pure $
      Exp.obtainCommonConstraints era $
        Exp.makeTransactionBodyAutoBalance
          offlineSystemStart
          offlineEpochInfo
          pparams
          mempty
          mempty
          ledgerUtxo
          content
          senderAddress
          Nothing

  let keyWitness = Exp.makeKeyWitness era unsignedTx senderWitness
  pure $ Exp.signTx era [] [keyWitness] unsignedTx

-- | Only used to convert slots to POSIX time for Plutus scripts; this
-- transaction has none, so any fixed epoch shape here is safe.
offlineSystemStart :: SystemStart
offlineSystemStart = SystemStart $ UTCTime (fromGregorian 2021 9 1) 0

-- | Only used by the auto-balancer to translate slots to wall-clock time for
-- Plutus scripts; this transaction has none, so a fixed shape avoids an extra RPC call.
offlineEpochInfo :: LedgerEpochInfo
offlineEpochInfo = LedgerEpochInfo $ fixedEpochInfo (EpochSize 1) (mkSlotLength 1)

mkLedgerTxOut :: Exp.Era era -> AddressInEra era -> Integer -> L.TxOut (Exp.LedgerEra era)
mkLedgerTxOut era address lovelace =
  Exp.obtainCommonConstraints era $
    L.mkBasicTxOut (toShelleyAddr address) (L.inject $ L.Coin lovelace)

-- | Poll the recipient's UTxOs until one matches the submitted transaction
-- hash, for up to a minute.
confirmDelivery
  :: Exp.Era era
  -- ^ The era the recipient address and submitted transaction belong to.
  -> Rpc.Connection
  -- ^ Open connection to cardano-rpc.
  -> AddressInEra era
  -- ^ Address to poll for the new UTxO.
  -> Text
  -- ^ The same address, pre-rendered for the confirmation message.
  -> BS.ByteString
  -- ^ Hash of the submitted transaction, to match against incoming UTxOs.
  -> IO ()
confirmDelivery era conn recipientAddress recipientAddressText submittedTxHash = go (20 :: Int)
 where
  go attemptsLeft
    | attemptsLeft <= 0 =
        throwString "Timed out waiting for the new UTxO to appear at the recipient address"
    | otherwise = do
        utxoItems <- searchUtxos era conn recipientAddress
        case listToMaybe [item | item <- utxoItems, item ^. Query.txoRef . Query.hash == submittedTxHash] of
          Just item -> do
            coin <- itemCoin item
            T.putStrLn $
              "Confirmed: "
                <> T.show coin
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

searchUtxos :: Exp.Era era -> Rpc.Connection -> AddressInEra era -> IO [Rpc.Proto Query.AnyUtxoData]
searchUtxos era conn address = do
  response <-
    Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf Query.QueryService "searchUtxos")) $
      defMessage & Query.predicate .~ exactAddressPredicate era address
  pure $ response ^. Query.items

-- | Fetch the current ledger protocol parameters via utxorpc's 'QueryService'
-- @readParams@ method (the same channel every other language example uses),
-- then map only the fields this script-free, single-payment transaction's
-- balancing consumes: the fee formula (per-byte and fixed) and the coins-per-
-- UTxO-byte rate the balancer needs for the change output's minimum UTxO.
-- 'Exp.makeTransactionBodyAutoBalance' never reads the maximum transaction
-- size at build time, so it is left unmapped; everything else (cost models,
-- deposits, governance thresholds, ...) stays at 'L.emptyPParams'' zero or
-- empty default, which this no-script, no-certificate transaction never touches.
readProtocolParams :: Exp.Era era -> Rpc.Connection -> IO (L.PParams (Exp.LedgerEra era))
readProtocolParams era conn = do
  response <-
    Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf Query.QueryService "readParams")) defMessage
  let pparams = response ^. Query.values . Query.cardano

  minFeeCoefficient <- bigIntToInteger (pparams ^. Query.minFeeCoefficient)
  minFeeConstant <- bigIntToInteger (pparams ^. Query.minFeeConstant)
  coinsPerUtxoByte <- bigIntToInteger (pparams ^. Query.coinsPerUtxoByte)
  minFeeCoefficientCompact <- toCompactCoin "minFeeCoefficient" minFeeCoefficient
  coinsPerUtxoByteCompact <- toCompactCoin "coinsPerUtxoByte" coinsPerUtxoByte

  pure $
    Exp.obtainCommonConstraints era $
      L.emptyPParams
        & L.ppTxFeePerByteL .~ L.CoinPerByte minFeeCoefficientCompact
        & L.ppTxFeeFixedL .~ L.Coin minFeeConstant
        & L.ppCoinsPerUTxOByteL .~ L.CoinPerByte coinsPerUtxoByteCompact

-- | Pack a non-negative lovelace amount into the ledger's compact coin
-- representation, as 'L.CoinPerByte' requires.
toCompactCoin
  :: String
  -- ^ Field name, used only to name the offending value in the error.
  -> Integer
  -> IO (L.CompactForm L.Coin)
toCompactCoin fieldName value =
  maybe
    (throwString $ "Protocol parameter " <> fieldName <> " does not fit a compact coin: " <> show value)
    (pure . L.CompactCoin)
    (L.integerToWord64 value)

itemCoin :: Rpc.Proto Query.AnyUtxoData -> IO Integer
itemCoin item = bigIntToInteger (item ^. Query.cardano . Query.coin)

itemToTxIn :: MonadThrow m => Rpc.Proto Query.TxoRef -> m TxIn
itemToTxIn txoRef =
  case deserialiseFromRawBytes AsTxId (txoRef ^. Query.hash) of
    Left err -> throwM . stringException $ "Malformed txo ref hash from cardano-rpc: " <> show err
    Right utxoTxId -> pure $ TxIn utxoTxId (TxIx (fromIntegral (txoRef ^. Query.index)))

-- | Decode a UTxO RPC 'Query.BigInt': its value is stored in one of three
-- oneof variants depending on magnitude and sign.
bigIntToInteger :: Rpc.Proto Query.BigInt -> IO Integer
bigIntToInteger bigInt
  | Just int <- bigInt ^. Query.maybe'int = pure $ toInteger int
  | Just bytes <- bigInt ^. Query.maybe'bigUInt = toInteger <$> naturalFromRawBytes bytes
  | Just bytes <- bigInt ^. Query.maybe'bigNInt = do
      n <- naturalFromRawBytes bytes
      pure $ negate (toInteger n) - 1
  | otherwise = pure 0
 where
  naturalFromRawBytes bytes =
    either (throwString . show) pure $ deserialiseFromRawBytes AsNatural bytes

-- | Match a UTxO exactly by its address, mirroring cardano-rpc's own
-- (internal) 'exactAddressPredicate'.
exactAddressPredicate :: Exp.Era era -> AddressInEra era -> Rpc.Proto Query.UtxoPredicate
exactAddressPredicate era address =
  Exp.obtainCommonConstraints era $
    defMessage
      & Query.match
        .~ ( defMessage
               & Query.cardano
                 .~ (defMessage & Query.address .~ (defMessage & Query.exactAddress .~ serialiseToRawBytes address))
           )

parseAddress :: Exp.Era era -> Text -> IO (AddressInEra era)
parseAddress era addressText =
  Exp.obtainCommonConstraints era $
    orDie id $
      maybe (Left $ "Could not parse address: " <> T.unpack addressText) Right $
        deserialiseAddress (AsAddressInEra asType) addressText

hexEncode :: BS.ByteString -> Text
hexEncode = T.decodeUtf8 . Base16.encode

orDie :: (e -> String) -> Either e a -> IO a
orDie render = either (throwString . render) pure
