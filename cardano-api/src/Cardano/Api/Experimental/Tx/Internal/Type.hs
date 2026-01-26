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
  ( UnsignedTx (..)
  )
where

import Cardano.Api.Experimental.Era
import Cardano.Api.HasTypeProxy (HasTypeProxy (..), Proxy, asType)
import Cardano.Api.Ledger.Internal.Reexport qualified as L
import Cardano.Api.ProtocolParameters
import Cardano.Api.Serialise.Raw
  ( SerialiseAsRawBytes (..)
  , SerialiseAsRawBytesError (SerialiseAsRawBytesError)
  )

import Cardano.Ledger.Binary qualified as Ledger
import Cardano.Ledger.Core qualified as Ledger

import Control.Exception (displayException)
import Data.Bifunctor (bimap)
import Data.ByteString.Lazy (fromStrict)

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
