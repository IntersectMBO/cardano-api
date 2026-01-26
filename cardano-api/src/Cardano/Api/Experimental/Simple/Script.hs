{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Api.Experimental.Simple.Script
  ( SimpleScript (..)
  , SimpleScriptOrReferenceInput (..)
  , deserialiseSimpleScript
  , hashSimpleScript
  )
where

import Cardano.Api.Experimental.Era
import Cardano.Api.HasTypeProxy
import Cardano.Api.Ledger.Internal.Reexport qualified as L
import Cardano.Api.Serialise.Cbor
import Cardano.Api.Tx.Internal.TxIn (TxIn)

import Cardano.Ledger.Alonzo.Scripts qualified as Alonzo
import Cardano.Ledger.Binary qualified as CBOR
import Cardano.Ledger.Core qualified as L

import Data.ByteString qualified as BS

-- | A simple script in a particular era. We leverage ledger's Cardano.Api.Experimental.ErasraScript
-- type class methods to work with the script.
data SimpleScript era where
  SimpleScript :: L.EraScript era => L.NativeScript era -> SimpleScript era

deriving instance Show (SimpleScript era)

deriving instance Eq (SimpleScript era)

instance L.Era era => HasTypeProxy (SimpleScript era) where
  data AsType (SimpleScript era) = AsSimpleScriptEra (Proxy era)
  proxyToAsType _ = AsSimpleScriptEra Proxy

instance
  (L.Era era, L.EraScript era)
  => SerialiseAsCBOR (SimpleScript era)
  where
  serialiseToCBOR (SimpleScript ns) = L.serialize' (L.eraProtVerHigh @era) ns

  deserialiseFromCBOR _ bs = do
    r <-
      CBOR.runAnnotator
        <$> CBOR.decodeFull' (L.eraProtVerHigh @era) bs
    return $ SimpleScript $ r $ CBOR.Full $ BS.fromStrict bs

-- TODO: We should also deserialize the JSON representation of simple scripts.
deserialiseSimpleScript
  :: forall era
   . L.EraScript era
  => BS.ByteString
  -> Either CBOR.DecoderError (SimpleScript era)
deserialiseSimpleScript bs =
  deserialiseFromCBOR (proxyToAsType (Proxy @(SimpleScript era))) bs

hashSimpleScript
  :: forall era. IsEra era => SimpleScript (LedgerEra era) -> L.ScriptHash
hashSimpleScript (SimpleScript ns) =
  case useEra @era of
    ConwayEra -> L.hashScript $ Alonzo.NativeScript ns
    DijkstraEra -> L.hashScript $ Alonzo.NativeScript ns

data SimpleScriptOrReferenceInput era
  = SScript (SimpleScript era)
  | SReferenceScript TxIn
  deriving (Show, Eq)
