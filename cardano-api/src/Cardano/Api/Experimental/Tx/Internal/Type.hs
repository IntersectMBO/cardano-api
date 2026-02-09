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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Api.Experimental.Tx.Internal.Type
  ( UnsignedTx (..)
  )
where

import Cardano.Api.Era.Internal.Core qualified as Api
import Cardano.Api.HasTypeProxy (HasTypeProxy (..), asType)
import Cardano.Api.Ledger.Internal.Reexport qualified as L
import Cardano.Api.ProtocolParameters
import Cardano.Api.Serialise.Raw
  ( SerialiseAsRawBytes (..)
  , SerialiseAsRawBytesError (SerialiseAsRawBytesError)
  )

import Cardano.Ledger.Api.Era qualified as L
import Cardano.Ledger.Binary qualified as Ledger
import Cardano.Ledger.Core qualified as Ledger

import Control.Exception (displayException)
import Data.Bifunctor (bimap)
import Data.ByteString.Lazy (fromStrict)
import Data.Foldable (asum)
import Data.Typeable

-- | A transaction that can contain everything
-- except key witnesses.
data UnsignedTx era
  = L.EraTx era => UnsignedTx (Ledger.Tx era)

type family ToApiEra ledgerera where
  ToApiEra L.DijkstraEra = Api.DijkstraEra
  ToApiEra L.ConwayEra = Api.ConwayEra
  ToApiEra L.BabbageEra = Api.BabbageEra
  ToApiEra L.AlonzoEra = Api.AlonzoEra
  ToApiEra L.MaryEra = Api.MaryEra
  ToApiEra L.AllegraEra = Api.AllegraEra
  ToApiEra L.ShelleyEra = Api.ShelleyEra
  ToApiEra L.ByronEra = Api.ByronEra

instance Typeable era => HasTypeProxy (UnsignedTx era) where
  data AsType (UnsignedTx era) = AsUnsignedTx (AsType (ToApiEra era))
  proxyToAsType :: Proxy (UnsignedTx era) -> AsType (UnsignedTx era)
  proxyToAsType p =
    let checkAllEras =
          asum
            [ isExpectedEra (Proxy @L.DijkstraEra) p
            , isExpectedEra (Proxy @L.ConwayEra) p
            , isExpectedEra (Proxy @L.BabbageEra) p
            , isExpectedEra (Proxy @L.AlonzoEra) p
            , isExpectedEra (Proxy @L.MaryEra) p
            , isExpectedEra (Proxy @L.AllegraEra) p
            , isExpectedEra (Proxy @L.ShelleyEra) p
            , isExpectedEra (Proxy @L.ByronEra) p
            ]
     in case checkAllEras of
          Just a -> a
          Nothing -> error "HasTypeProxy (UnsignedTx era): Era not supported"

isExpectedEra
  :: forall expected actual ledgerera
   . (Typeable expected, Typeable actual, HasTypeProxy ledgerera, ToApiEra expected ~ ledgerera)
  => Proxy expected -> Proxy (UnsignedTx actual) -> Maybe (AsType (UnsignedTx actual))
isExpectedEra _ _ = case eqT @actual @expected of
  Just Refl -> Just (AsUnsignedTx (asType @(ToApiEra expected)))
  Nothing -> Nothing

instance
  L.EraTx era
  => SerialiseAsRawBytes (UnsignedTx era)
  where
  serialiseToRawBytes (UnsignedTx tx) =
    Ledger.serialize' (Ledger.eraProtVerHigh @era) tx
  deserialiseFromRawBytes _ =
    bimap wrapError UnsignedTx
      . Ledger.decodeFullAnnotator
        (Ledger.eraProtVerHigh @era)
        "UnsignedTx"
        Ledger.decCBOR
      . fromStrict
   where
    wrapError
      :: Ledger.DecoderError -> SerialiseAsRawBytesError
    wrapError = SerialiseAsRawBytesError . displayException

deriving instance Eq (UnsignedTx era)

deriving instance Show (UnsignedTx era)
