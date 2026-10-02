{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Which Plutus languages each ledger era accepts, as a type-level table.
--
-- The table mirrors ledger's 'L.eraMaxLanguage' and must be extended when a new
-- era or Plutus language lands. It is consulted in two ways:
--
-- * At compile time, 'PlutusLangInEra' is a constraint on the 'PlutusScriptInEra'
--   constructor. A pairing the table does not list is a type error, so a script
--   with an unsupported language cannot be placed in a transaction for that era.
--
-- * At runtime, 'plutusLangInEra' reflects the table back as a value. Code that
--   only learns the language at runtime, such as a deserialiser, uses it to
--   obtain the evidence the constructor demands. GHC checks each row of
--   'plutusLangInEra' against the type family, so the two cannot disagree.
module Cardano.Api.Experimental.Plutus.Internal.Language
  ( PlutusLangInEra
  , PlutusLangInEraEvidence (..)
  , PlutusLangsInEra (..)
  )
where

import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Plutus.Language qualified as L

import Data.Kind (Constraint, Type)
import GHC.TypeLits (ErrorMessage (..), TypeError)

-- | Holds when the ledger era @era@ accepts Plutus scripts of language @lang@.
type family PlutusLangInEra (lang :: L.Language) (era :: Type) :: Constraint where
  PlutusLangInEra 'L.PlutusV1 L.AlonzoEra = ()
  PlutusLangInEra 'L.PlutusV1 L.BabbageEra = ()
  PlutusLangInEra 'L.PlutusV2 L.BabbageEra = ()
  PlutusLangInEra 'L.PlutusV1 L.ConwayEra = ()
  PlutusLangInEra 'L.PlutusV2 L.ConwayEra = ()
  PlutusLangInEra 'L.PlutusV3 L.ConwayEra = ()
  PlutusLangInEra 'L.PlutusV1 L.DijkstraEra = ()
  PlutusLangInEra 'L.PlutusV2 L.DijkstraEra = ()
  PlutusLangInEra 'L.PlutusV3 L.DijkstraEra = ()
  PlutusLangInEra 'L.PlutusV4 L.DijkstraEra = ()
  PlutusLangInEra lang era =
    TypeError
      ( 'Text "Plutus language "
          ':<>: 'ShowType lang
          ':<>: 'Text " is not supported in the ledger era "
          ':<>: 'ShowType era
      )

-- | Runtime evidence that 'PlutusLangInEra' holds. Pattern matching on the
-- constructor brings the constraint into scope.
data PlutusLangInEraEvidence (lang :: L.Language) era where
  PlutusLangInEraEvidence :: PlutusLangInEra lang era => PlutusLangInEraEvidence lang era

-- | The value-level reflection of 'PlutusLangInEra', one instance per ledger era.
class PlutusLangsInEra era where
  plutusLangInEra :: L.SLanguage lang -> Maybe (PlutusLangInEraEvidence lang era)

instance PlutusLangsInEra L.AlonzoEra where
  plutusLangInEra = \case
    L.SPlutusV1 -> Just PlutusLangInEraEvidence
    L.SPlutusV2 -> Nothing
    L.SPlutusV3 -> Nothing
    L.SPlutusV4 -> Nothing

instance PlutusLangsInEra L.BabbageEra where
  plutusLangInEra = \case
    L.SPlutusV1 -> Just PlutusLangInEraEvidence
    L.SPlutusV2 -> Just PlutusLangInEraEvidence
    L.SPlutusV3 -> Nothing
    L.SPlutusV4 -> Nothing

instance PlutusLangsInEra L.ConwayEra where
  plutusLangInEra = \case
    L.SPlutusV1 -> Just PlutusLangInEraEvidence
    L.SPlutusV2 -> Just PlutusLangInEraEvidence
    L.SPlutusV3 -> Just PlutusLangInEraEvidence
    L.SPlutusV4 -> Nothing

instance PlutusLangsInEra L.DijkstraEra where
  plutusLangInEra = \case
    L.SPlutusV1 -> Just PlutusLangInEraEvidence
    L.SPlutusV2 -> Just PlutusLangInEraEvidence
    L.SPlutusV3 -> Just PlutusLangInEraEvidence
    L.SPlutusV4 -> Just PlutusLangInEraEvidence
