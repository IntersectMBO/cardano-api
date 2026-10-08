{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

-- | Which Plutus languages each ledger era accepts.
--
-- The table is the ledger's 'L.EraPlutusTxInfo' instances, one per language and era it supports.
-- The runtime lookup is the ledger's 'L.mkSupportedLanguage'.
module Cardano.Api.Experimental.Plutus.Internal.Language
  ( PlutusLangInEra (..)
  , HasPlutusLangInEra (..)
  , plutusLangInShelleyBasedEra
  )
where

import Cardano.Api.Era.Internal.Eon.ShelleyBasedEra (ShelleyBasedEra (..), ShelleyLedgerEra)

import Cardano.Ledger.Alonzo.Plutus.Context qualified as L
  ( EraPlutusContext (..)
  , EraPlutusTxInfo
  , SupportedLanguage (..)
  )
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Plutus.Language qualified as L

import Data.Typeable (eqT, (:~:) (Refl))

-- | Evidence that the ledger era @era@ accepts Plutus scripts of @lang@.
-- The ledger's 'L.EraPlutusTxInfo' instances are the table; matching gives 'L.AlonzoEraScript'.
data PlutusLangInEra (lang :: L.Language) era where
  PlutusLangInEra :: L.EraPlutusTxInfo lang era => L.SLanguage lang -> PlutusLangInEra lang era

deriving instance Show (PlutusLangInEra lang era)

deriving instance Eq (PlutusLangInEra lang era)

-- | The runtime side of the table, for code that learns the language late.
plutusLangInShelleyBasedEra
  :: L.PlutusLanguage lang
  => ShelleyBasedEra era
  -> L.SLanguage lang
  -> Maybe (PlutusLangInEra lang (ShelleyLedgerEra era))
plutusLangInShelleyBasedEra sbe slang = case sbe of
  ShelleyBasedEraShelley -> Nothing
  ShelleyBasedEraAllegra -> Nothing
  ShelleyBasedEraMary -> Nothing
  ShelleyBasedEraAlonzo -> supportedLanguageProof slang
  ShelleyBasedEraBabbage -> supportedLanguageProof slang
  ShelleyBasedEraConway -> supportedLanguageProof slang
  ShelleyBasedEraDijkstra -> supportedLanguageProof slang

-- | Ask the ledger whether the era runs the language and keep its proof.
supportedLanguageProof
  :: forall era lang
   . (L.EraPlutusContext era, L.PlutusLanguage lang)
  => L.SLanguage lang -> Maybe (PlutusLangInEra lang era)
supportedLanguageProof slang = do
  L.SupportedLanguage (_ :: L.SLanguage l) <- L.mkSupportedLanguage @era (L.plutusLanguage slang)
  Refl <- eqT @lang @l
  pure $ PlutusLangInEra slang

-- | Hands out a 'PlutusLangInEra' proof for code that only has the ledger era type,
-- by asking the ledger's 'L.mkSupportedLanguage'. Eras before Alonzo always give 'Nothing'.
--
-- With the language known only at the type level, take the singleton from 'L.PlutusLanguage':
--
-- > case plutusLangInEra @era (L.isLanguage @lang) of
-- >   Just (PlutusLangInEra _) -> -- the era runs the language, 'L.EraPlutusTxInfo' is in scope
-- >   Nothing -> -- it does not
--
-- When the pairing is static and supported, 'L.EraPlutusTxInfo' already holds and the proof
-- needs no lookup:
--
-- > PlutusLangInEra L.isLanguage :: PlutusLangInEra lang era
class HasPlutusLangInEra era where
  plutusLangInEra :: L.PlutusLanguage lang => L.SLanguage lang -> Maybe (PlutusLangInEra lang era)

instance HasPlutusLangInEra L.ShelleyEra where
  plutusLangInEra = plutusLangInShelleyBasedEra ShelleyBasedEraShelley

instance HasPlutusLangInEra L.AllegraEra where
  plutusLangInEra = plutusLangInShelleyBasedEra ShelleyBasedEraAllegra

instance HasPlutusLangInEra L.MaryEra where
  plutusLangInEra = plutusLangInShelleyBasedEra ShelleyBasedEraMary

instance HasPlutusLangInEra L.AlonzoEra where
  plutusLangInEra = plutusLangInShelleyBasedEra ShelleyBasedEraAlonzo

instance HasPlutusLangInEra L.BabbageEra where
  plutusLangInEra = plutusLangInShelleyBasedEra ShelleyBasedEraBabbage

instance HasPlutusLangInEra L.ConwayEra where
  plutusLangInEra = plutusLangInShelleyBasedEra ShelleyBasedEraConway

instance HasPlutusLangInEra L.DijkstraEra where
  plutusLangInEra = plutusLangInShelleyBasedEra ShelleyBasedEraDijkstra
