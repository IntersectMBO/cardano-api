module Test.Cardano.Rpc.ByronTx where

import Cardano.Api.Address (Address (..))
import Cardano.Api.Serialise.Raw (serialiseToRawBytes)
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c
import Cardano.Rpc.Server.Internal.UtxoRpc.Byron (byronTxToUtxoRpcTx)
import Cardano.Rpc.Server.Internal.UtxoRpc.Type (utxoRpcBigIntToInteger)

import Cardano.Chain.Common (lovelaceToInteger)
import Cardano.Chain.UTxO
  ( ATxAux (..)
  , Tx (..)
  , TxIn (..)
  , TxInWitness (..)
  , TxOut (..)
  , annotateTxAux
  , taTx
  , taWitness
  )
import Cardano.Crypto qualified as Byron
  ( RedeemSignature (..)
  , RedeemVerificationKey (..)
  , Signature (..)
  , VerificationKey (..)
  , fromVerificationKeyToByteString
  , hashRaw
  , hashToBytes
  , serializeCborHash
  )
import Cardano.Crypto.Wallet qualified as WC
import Cardano.Ledger.Binary (Annotated (..))

import RIO

import Data.ByteArray qualified as BA
import Data.ByteString.Lazy qualified as LBS
import Data.ProtoLens (decodeMessage, encodeMessage)
import Network.GRPC.Spec (Proto (..))

import Test.Cardano.Chain.UTxO.Gen (genTxAux)
import Test.Cardano.Crypto.Gen (genProtocolMagicId)

import Hedgehog as H
import Hedgehog.Extras qualified as H

-- | One-way property for 'byronTxToUtxoRpcTx': there is no inverse
-- conversion, so the proto message and the Byron transaction are projected
-- onto comparable facts which must agree. The generated transaction is
-- re-annotated through CBOR ('annotateTxAux') so the conversion sees the
-- same annotated bytes a transaction fetched from a block would carry.
hprop_byron_tx_to_utxorpc_tx :: Property
hprop_byron_tx_to_utxorpc_tx = H.property $ do
  protocolMagicId <- forAll genProtocolMagicId
  txAux <- forAll $ annotateTxAux <$> genTxAux protocolMagicId
  let protoTx = byronTxToUtxoRpcTx txAux
      tx = taTx txAux
      witnesses = toList (taWitness txAux)

  H.note_ "Wire-level protobuf roundtrip, forcing the full message"
  decodeMessage (encodeMessage protoTx) === Right protoTx

  H.note_ "Transaction hash equals the Byron transaction id"
  protoTx ^. U5c.hash === Byron.hashToBytes (Byron.serializeCborHash tx)

  -- 'annotateTxAux' makes the annotation equal to the canonical re-encoding,
  -- so the assertion above alone cannot distinguish hashing the annotated
  -- bytes from re-serialising. Forging the annotation pins the former.
  H.note_ "The hash follows the annotated bytes, not a re-serialisation"
  let bogusBytes = "definitely not the canonical encoding" :: ByteString
  byronTxToUtxoRpcTx txAux{aTaTx = Annotated tx bogusBytes} ^. U5c.hash
    === Byron.hashToBytes (Byron.hashRaw (LBS.fromStrict bogusBytes))

  H.note_ "Inputs preserve order, transaction hash and output index"
  let protoInputRefs :: [(ByteString, Word32)]
      protoInputRefs =
        protoTx ^. U5c.inputs <&> \input -> (input ^. U5c.txHash, input ^. U5c.outputIndex)
  protoInputRefs
    === ( toList (txInputs tx) <&> \(TxInUtxo txId txIx) ->
            (Byron.hashToBytes txId, fromIntegral txIx)
        )

  H.note_ "Outputs preserve order, raw address bytes and coin"
  let protoOutputs :: [U5c.TxOutput]
      protoOutputs = protoTx ^. U5c.outputs
  length protoOutputs === length (txOutputs tx)
  forM_ (zip protoOutputs (toList (txOutputs tx))) $ \(protoOutput, TxOut address value) -> do
    protoOutput ^. U5c.address === serialiseToRawBytes (ByronAddress address)
    coin <- utxoRpcBigIntToInteger . Proto $ protoOutput ^. U5c.coin
    coin === lovelaceToInteger value

  H.note_ "Witness arm routing: VKWitness -> bootstrap, RedeemWitness -> vkey"
  let protoWitnessSet :: U5c.WitnessSet
      protoWitnessSet = protoTx ^. U5c.witnesses
      vkWitnesses = [(vk, sig) | VKWitness vk (Byron.Signature sig) <- witnesses]
      redeemWitnesses =
        [ (rk, rsig)
        | RedeemWitness (Byron.RedeemVerificationKey rk) (Byron.RedeemSignature rsig) <- witnesses
        ]
  length (protoWitnessSet ^. U5c.bootstrapWitnesses) === length vkWitnesses
  length (protoWitnessSet ^. U5c.vkeywitness) === length redeemWitnesses

  H.note_ "Bootstrap witnesses carry the split XPub and the raw XSignature"
  forM_ (zip (protoWitnessSet ^. U5c.bootstrapWitnesses) vkWitnesses) $
    \( protoWitness
       , (Byron.VerificationKey (WC.XPub vkeyBytes (WC.ChainCode chainCodeBytes)), xSignature)
       ) -> do
        protoWitness ^. U5c.vkey === vkeyBytes
        protoWitness ^. U5c.chainCode === chainCodeBytes
        protoWitness ^. U5c.signature === WC.unXSignature xSignature
        protoWitness ^. U5c.attributes === mempty

  H.note_ "VKey witnesses carry the raw Ed25519 redeem key and signature"
  forM_ (zip (protoWitnessSet ^. U5c.vkeywitness) redeemWitnesses) $
    \(protoWitness, (redeemKey, redeemSignature)) -> do
      protoWitness ^. U5c.vkey === Byron.fromVerificationKeyToByteString redeemKey
      protoWitness ^. U5c.signature === BA.convert redeemSignature

  H.note_ "Fee is unset: Byron fees are implicit"
  protoTx ^. U5c.maybe'fee === Nothing

  H.note_ "Fields with no Byron counterpart stay empty"
  protoTx ^. U5c.certificates === []
  protoTx ^. U5c.withdrawals === []
  protoTx ^. U5c.mint === []
  protoTx ^. U5c.referenceInputs === []
  protoTx ^. U5c.proposals === []
  protoTx ^. U5c.maybe'validity === Nothing
  protoTx ^. U5c.maybe'collateral === Nothing
  protoTx ^. U5c.maybe'auxiliary === Nothing
  protoWitnessSet ^. U5c.script === []
  protoWitnessSet ^. U5c.plutusDatums === []
  protoWitnessSet ^. U5c.redeemers === []

  H.note_ "Byron has no phase-2 validation, so transactions are always successful"
  H.assertWith protoTx (^. U5c.successful)
