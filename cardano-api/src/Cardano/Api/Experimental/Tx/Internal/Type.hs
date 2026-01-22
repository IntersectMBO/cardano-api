{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Api.Experimental.Tx.Internal.Type
  ( SignedTx (..)
  , UnsignedTx (..)
  )
where

import Cardano.Api.Experimental.Era
import Cardano.Api.HasTypeProxy (HasTypeProxy (..), Proxy, asType)
import Cardano.Api.Ledger.Internal.Reexport qualified as L
import Cardano.Api.Serialise.Cbor
import Cardano.Api.Serialise.Raw
  ( SerialiseAsRawBytes (..)
  , SerialiseAsRawBytesError (SerialiseAsRawBytesError)
  )
import Cardano.Api.Serialise.TextEnvelope.Internal

import Cardano.Ledger.Binary qualified as Ledger
import Cardano.Ledger.Core qualified as Ledger

import Control.Exception (displayException)
import Data.Bifunctor (bimap)
import Data.ByteString.Lazy (fromStrict)

-- | A transaction that has been witnesssed
data SignedTx era
  = L.EraTx (LedgerEra era) => SignedTx (Ledger.Tx (LedgerEra era))

deriving instance Eq (SignedTx era)

deriving instance Show (SignedTx era)

instance
  ( HasTypeProxy era
  , L.EraTx (LedgerEra era)
  )
  => SerialiseAsCBOR (SignedTx era)
  where
  serialiseToCBOR (SignedTx tx) =
    Ledger.serialize' (Ledger.eraProtVerHigh @(LedgerEra era)) tx
  deserialiseFromCBOR _ =
    fmap SignedTx
      . Ledger.decodeFullAnnotator
        (Ledger.eraProtVerHigh @(LedgerEra era))
        "UnsignedTx"
        Ledger.decCBOR
      . fromStrict

instance (L.EraTx (LedgerEra era), HasTypeProxy era) => HasTextEnvelope (SignedTx era) where
  textEnvelopeType _ = "Tx"

instance HasTypeProxy era => HasTypeProxy (SignedTx era) where
  data AsType (SignedTx era) = AsSignedTx (AsType era)
  proxyToAsType :: Proxy (SignedTx era) -> AsType (SignedTx era)
  proxyToAsType _ = AsSignedTx (asType @era)

instance
  ( HasTypeProxy era
  , L.EraTx (LedgerEra era)
  )
  => SerialiseAsRawBytes (SignedTx era)
  where
  serialiseToRawBytes (SignedTx tx) =
    Ledger.serialize' (Ledger.eraProtVerHigh @(LedgerEra era)) tx
  deserialiseFromRawBytes _ =
    bimap wrapError SignedTx
      . Ledger.decodeFullAnnotator
        (Ledger.eraProtVerHigh @(LedgerEra era))
        "SignedTx"
        Ledger.decCBOR
      . fromStrict
   where
    wrapError
      :: Ledger.DecoderError -> SerialiseAsRawBytesError
    wrapError = SerialiseAsRawBytesError . displayException

-- | A transaction that can contain everything
-- except key witnesses.
data UnsignedTx era
  = L.EraTx (LedgerEra era) => UnsignedTx (Ledger.Tx (LedgerEra era))

instance HasTypeProxy era => HasTypeProxy (UnsignedTx era) where
  data AsType (UnsignedTx era) = AsUnsignedTx (AsType era)
  proxyToAsType :: Proxy (UnsignedTx era) -> AsType (UnsignedTx era)
  proxyToAsType _ = AsUnsignedTx (asType @era)

instance
  ( HasTypeProxy era
  , L.EraTx (LedgerEra era)
  )
  => SerialiseAsRawBytes (UnsignedTx era)
  where
  serialiseToRawBytes (UnsignedTx tx) =
    Ledger.serialize' (Ledger.eraProtVerHigh @(LedgerEra era)) tx
  deserialiseFromRawBytes _ =
    bimap wrapError UnsignedTx
      . Ledger.decodeFullAnnotator
        (Ledger.eraProtVerHigh @(LedgerEra era))
        "UnsignedTx"
        Ledger.decCBOR
      . fromStrict
   where
    wrapError
      :: Ledger.DecoderError -> SerialiseAsRawBytesError
    wrapError = SerialiseAsRawBytesError . displayException

deriving instance Eq (UnsignedTx era)

deriving instance Show (UnsignedTx era)
