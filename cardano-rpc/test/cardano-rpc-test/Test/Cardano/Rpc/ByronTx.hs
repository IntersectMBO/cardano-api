module Test.Cardano.Rpc.ByronTx where

import Cardano.Api.Address (Address (..))
import Cardano.Api.Serialise.Raw (serialiseToRawBytes)
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c
import Cardano.Rpc.Server.Internal.UtxoRpc.Type (utxoRpcBigIntToInteger)
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.Byron (byronBlockTxs, byronTxToUtxoRpcTx)

import Cardano.Chain.Block qualified as Byron
  ( ABlockOrBoundary (..)
  , blockHashAnnotated
  , boundaryHeader
  , boundaryHeaderHashAnnotated
  , decCBORABlockOrBoundary
  )
import Cardano.Chain.Common (lovelaceToInteger)
import Cardano.Chain.Epoch.File (mainnetEpochSlots)
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
  ( Hash
  , RedeemSignature (..)
  , RedeemVerificationKey (..)
  , Signature (..)
  , VerificationKey (..)
  , decodeHash
  , fromVerificationKeyToByteString
  , hashRaw
  , hashToBytes
  , serializeCborHash
  )
import Cardano.Crypto.Wallet qualified as WC
import Cardano.Ledger.Binary (Annotated (..), byronProtVer, decodeFullDecoder, slice)

import RIO

import Codec.Compression.GZip qualified as GZip
import Data.ByteArray qualified as BA
import Data.ByteString.Lazy qualified as LBS
import Data.ProtoLens (decodeMessage, encodeMessage)
import Network.GRPC.Spec (Proto)

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
  let protoOutputs :: [Proto U5c.TxOutput]
      protoOutputs = protoTx ^. U5c.outputs
  length protoOutputs === length (txOutputs tx)
  forM_ (zip protoOutputs (toList (txOutputs tx))) $ \(protoOutput, TxOut address value) -> do
    protoOutput ^. U5c.address === serialiseToRawBytes (ByronAddress address)
    coin <- utxoRpcBigIntToInteger $ protoOutput ^. U5c.coin
    coin === lovelaceToInteger value

  H.note_ "Witness arm routing: VKWitness -> bootstrap, RedeemWitness -> vkey"
  let protoWitnessSet :: Proto U5c.WitnessSet
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

-- | Golden test for 'byronBlockTxs' on a pre-verified mainnet main block
-- fixture, stored in the exact 'Byron.ABlockOrBoundary' wire encoding the
-- node's ImmutableDB keeps: block 2,160,150 (epoch 100, absolute slot
-- 2,161,637), carrying six transactions. The expected header hash and
-- transaction ids were verified cryptographically against mainnet when the
-- fixture was retrieved.
hprop_byron_main_block_txs_golden :: Property
hprop_byron_main_block_txs_golden = H.propertyOnce $ do
  let decodeTxIdHex :: Text -> Either Text (Byron.Hash Tx)
      decodeTxIdHex = Byron.decodeHash

  H.note_ "The main block fixture decodes and its header hash matches mainnet"
  mainBlockBytes <-
    H.evalIO $ LBS.readFile "test/cardano-rpc-test/files/golden/byron-main-block.cbor"
  mainBlock <- decodeBlockOrBoundaryFixture mainBlockBytes
  expectedMainHash <-
    H.leftFail $
      Byron.decodeHash "e0fc5d1bb892af22727651fb7ccc2c6465395a491174b2630e4b525cacf1859d"
  case mainBlock of
    Byron.ABOBBlock byronBlock -> Byron.blockHashAnnotated byronBlock === expectedMainHash
    Byron.ABOBBoundary _ -> do
      H.note_ "expected a main block fixture, decoded a boundary block"
      H.failure

  H.note_ "The conversion yields the six transactions of the block, in order"
  let mainBlockTxs :: [Proto U5c.Tx]
      mainBlockTxs = byronBlockTxs mainBlock
  expectedTxIds <-
    H.leftFail $
      traverse
        decodeTxIdHex
        [ "d2937ec5576f63094d066a3ad93997bf55e651d76b7dc4082a2cef5a85eae657"
        , "649756d9194ecd79b017fa4753748d44da0d231d95b8f2b4a8e44d2cbab529c8"
        , "c6304ae76fe6a0493f5236dee75b42b9ec33cd62782f2db455a36e40143a1d12"
        , "734932fb26bbd9ec29d4f904285e6623e11e7a9751675f761c3b53a04f0c1a68"
        , "c2ae415049dbc046c5650e6ba74e52dd2eef87f31f3712ccc220aa26c51a097a"
        , "e54e392d7ed0f79bdff0342b2588be3eb899ca997364812e31651b4346038b1d"
        ]
  length mainBlockTxs === 6
  (mainBlockTxs <&> (^. U5c.hash)) === (Byron.hashToBytes <$> expectedTxIds)

  H.note_ "The first transaction carries inputs, outputs and witnesses, and no fee"
  firstTx <- H.nothingFail $ listToMaybe mainBlockTxs
  H.assertWith firstTx $ not . null . (^. U5c.inputs)
  H.assertWith firstTx $ not . null . (^. U5c.outputs)
  H.assertWith firstTx $ \protoTx ->
    not (null (protoTx ^. U5c.witnesses . U5c.bootstrapWitnesses))
      || not (null (protoTx ^. U5c.witnesses . U5c.vkeywitness))
  firstTx ^. U5c.maybe'fee === Nothing

-- | Golden test for 'byronBlockTxs' on a pre-verified mainnet epoch-100
-- boundary block fixture, stored in the exact 'Byron.ABlockOrBoundary' wire
-- encoding the node's ImmutableDB keeps. The fixture is gzip-compressed
-- because every mainnet boundary block carries the leader schedule. The
-- expected header hash was verified cryptographically against mainnet when
-- the fixture was retrieved.
hprop_byron_boundary_block_txs_golden :: Property
hprop_byron_boundary_block_txs_golden = H.propertyOnce $ do
  H.note_ "The boundary block fixture decodes and its header hash matches mainnet"
  compressedBoundaryBytes <-
    H.evalIO $ LBS.readFile "test/cardano-rpc-test/files/golden/byron-ebb.cbor.gz"
  boundaryBlock <- decodeBlockOrBoundaryFixture $ GZip.decompress compressedBoundaryBytes
  expectedBoundaryHash <-
    H.leftFail $
      Byron.decodeHash "46d133e7f2c90ce1117e653e8e7b6734f4624b76b746d1435e5b58fb8407955c"
  case boundaryBlock of
    Byron.ABOBBoundary ebb ->
      Byron.boundaryHeaderHashAnnotated (Byron.boundaryHeader ebb) === expectedBoundaryHash
    Byron.ABOBBlock _ -> do
      H.note_ "expected a boundary block fixture, decoded a main block"
      H.failure

  H.note_ "Boundary blocks carry no transactions"
  byronBlockTxs boundaryBlock === []

-- | Decode a 'Byron.ABlockOrBoundary' fixture from the wire encoding the
-- node's ImmutableDB stores, annotating the result with the original bytes,
-- exactly as a block fetched from the node would carry them.
decodeBlockOrBoundaryFixture
  :: HasCallStack
  => MonadTest m
  => LBS.ByteString
  -> m (Byron.ABlockOrBoundary ByteString)
decodeBlockOrBoundaryFixture blockBytes = do
  blockOrBoundary <-
    H.leftFail $
      decodeFullDecoder
        byronProtVer
        "ABlockOrBoundary"
        (Byron.decCBORABlockOrBoundary mainnetEpochSlots)
        blockBytes
  pure $ LBS.toStrict . slice blockBytes <$> blockOrBoundary
