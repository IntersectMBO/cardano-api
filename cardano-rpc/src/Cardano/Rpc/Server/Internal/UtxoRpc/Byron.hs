-- | Conversion of Byron-era transactions to UTxO RPC messages.
--
-- cardano-api's 'Cardano.Api.Tx.Tx' GADT has no Byron constructor, so the
-- conversion works directly on the Byron ledger types.
module Cardano.Rpc.Server.Internal.UtxoRpc.Byron
  ( byronTxToUtxoRpcTx
  )
where

import Cardano.Api.Address (Address (..))
import Cardano.Api.Era (Inject (..))
import Cardano.Api.Serialise.Raw
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as UtxoRpc
import Cardano.Rpc.Server.Internal.Orphans ()

import Cardano.Chain.Common (lovelaceToInteger)
import Cardano.Chain.UTxO
  ( ATxAux (..)
  , Tx (..)
  , TxIn (..)
  , TxInWitness (..)
  , TxOut (..)
  , taTx
  , taWitness
  )
import Cardano.Crypto qualified as Byron
  ( RedeemSignature (..)
  , RedeemVerificationKey (..)
  , Signature (..)
  , fromVerificationKeyToByteString
  , hashDecoded
  , hashToBytes
  )
import Cardano.Crypto.DSIGN.Class qualified as DSIGN
import Cardano.Crypto.Wallet qualified as WC
import Cardano.Ledger.Keys qualified as L (VKey (..))
import Cardano.Ledger.Keys.Bootstrap qualified as L (ChainCode (..), unpackByronVKey)

import RIO

import Data.ByteArray qualified as BA
import Data.ProtoLens (defMessage)
import Network.GRPC.Spec

-- | Convert a Byron-era transaction to the UTxO RPC 'UtxoRpc.Tx' message.
-- Populates hash, inputs, outputs, witnesses and @successful@.
--
-- The transaction hash covers the original on-chain annotated bytes
-- ('Byron.hashDecoded'); re-serialising could produce a different encoding
-- and therefore a wrong transaction id.
--
-- The @fee@ field is left unset: Byron fees are implicit (inputs minus
-- outputs) and cannot be recovered without resolving the inputs against the
-- UTxO set. @successful@ is always true because Byron has no phase-2
-- validation. All remaining fields (certificates, withdrawals, minting,
-- validity, collateral, reference inputs, auxiliary data and proposals) have
-- no Byron counterpart and stay empty.
--
-- Byron pairs witness @i@ with input @i@ positionally; splitting the
-- witnesses into the proto @bootstrapWitnesses@ and @vkeywitness@ arms does
-- not preserve that pairing, and it cannot be reconstructed from the address
-- alone. Consumers needing per-input authorisation must use @native_bytes@.
byronTxToUtxoRpcTx :: ATxAux ByteString -> UtxoRpc.Tx
byronTxToUtxoRpcTx txAux = do
  let tx = taTx txAux
      inputs :: [UtxoRpc.TxInput]
      inputs =
        toList (txInputs tx) <&> \(TxInUtxo txId txIx) ->
          defMessage
            & U5c.txHash .~ Byron.hashToBytes txId
            & U5c.outputIndex .~ fromIntegral txIx
      outputs :: [UtxoRpc.TxOutput]
      outputs =
        toList (txOutputs tx) <&> \(TxOut address value) ->
          defMessage
            & U5c.address .~ serialiseToRawBytes (ByronAddress address)
            & U5c.coin .~ getProto (inject (lovelaceToInteger value))
      witnesses = toList (taWitness txAux)
      -- a 'VKWitness' carries an extended public key, which is what the
      -- bootstrap witness shape (vkey + chain code) models; the attributes
      -- stay empty because Byron witnesses carry none (address attributes
      -- live in the address itself)
      bootstrapWitnesses :: [UtxoRpc.BootstrapWitness]
      bootstrapWitnesses =
        [ defMessage
            & U5c.vkey .~ DSIGN.rawSerialiseVerKeyDSIGN vkey
            & U5c.signature .~ WC.unXSignature xSignature
            & U5c.chainCode .~ L.unChainCode chainCode
        | VKWitness verificationKey (Byron.Signature xSignature) <- witnesses
        , let (L.VKey vkey, chainCode) = L.unpackByronVKey verificationKey
        ]
      -- a 'RedeemWitness' is a plain Ed25519 key pair
      vkeyWitnesses :: [UtxoRpc.VKeyWitness]
      vkeyWitnesses =
        [ defMessage
            & U5c.vkey .~ Byron.fromVerificationKeyToByteString redeemKey
            & U5c.signature .~ BA.convert redeemSignature
        | RedeemWitness
            (Byron.RedeemVerificationKey redeemKey)
            (Byron.RedeemSignature redeemSignature) <-
            witnesses
        ]
  defMessage
    & U5c.hash .~ Byron.hashToBytes (Byron.hashDecoded (aTaTx txAux))
    & U5c.inputs .~ inputs
    & U5c.outputs .~ outputs
    & U5c.witnesses
      .~ ( defMessage
             & U5c.vkeywitness .~ vkeyWitnesses
             & U5c.bootstrapWitnesses .~ bootstrapWitnesses
         )
    & U5c.successful .~ True
