{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Showcase for era ranges ("Cardano.Api.Experimental.Era.Range"): a lower
-- bounded range, an upper bounded range, narrowing a wider range to a
-- subset, and nested splits. Certificates make good examples because
-- certificate support genuinely fragments across eras.
module Test.Cardano.Api.Experimental.EraRange
  ( tests
  )
where

import Cardano.Api
  ( AlonzoEra
  , AnyCardanoEra (..)
  , BabbageEra
  , CardanoEra (..)
  , ConwayEra
  , DijkstraEra
  , IsShelleyBasedEra (..)
  , MaryEra
  , ShelleyEra
  , ShelleyLedgerEra
  )
import Cardano.Api.Experimental (Some (..))
import Cardano.Api.Experimental.Era.Range
import Cardano.Api.Experimental.Era.Range.Some

import Cardano.Crypto.Hash.Class qualified as Hash
import Cardano.Ledger.Address qualified as L (Addr (..))
import Cardano.Ledger.Api.Tx.Cert qualified as L
import Cardano.Ledger.BaseTypes qualified as L (Network (..))
import Cardano.Ledger.Coin qualified as L (Coin (..))
import Cardano.Ledger.Conway.TxCert qualified as L (ConwayEraTxCert (..))
import Cardano.Ledger.Core qualified as L (EraTxOut, mkCoinTxOut)
import Cardano.Ledger.Credential qualified as L
import Cardano.Ledger.Hashes qualified as L
import Cardano.Ledger.Shelley.TxCert qualified as L (ShelleyEraTxCert (..))

import Control.Monad (forM_)
import Data.ByteString qualified as BS
import Data.Function ((&))
import Data.List (isPrefixOf, partition)
import Data.Maybe (isJust, isNothing)

import Hedgehog (MonadTest, Property, (===))
import Hedgehog.Extras qualified as H
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

-- * Constraint bundles

--
-- A bundle is written exactly once, as a plain constraint alias with the
-- constraints in their natural applied form - the single source of truth,
-- usable applied in signatures. The class and its universal instance are a
-- two-line eta-expansion that only REFERENCE the alias: they exist because
-- 'withRange' takes its constraint parameter unapplied, and type synonyms
-- cannot be passed unsaturated, while a class name can.

-- Note: @Show (L.TxCert (ShelleyLedgerEra era))@ and friends are deliberately
-- absent from the bundles - ledger classes carry 'Show'\/'Eq'\/CBOR
-- superclasses on their associated types, so listing them explicitly would
-- trigger @-Wredundant-constraints@ in the leaf signatures below.

type ConwayCertConstraints era =
  ( IsShelleyBasedEra era
  , L.ConwayEraTxCert (ShelleyLedgerEra era)
  , L.EraTxOut (ShelleyLedgerEra era)
  )

class ConwayCertConstraints era => ConwayCertsC era

instance ConwayCertConstraints era => ConwayCertsC era

type LegacyCertConstraints era =
  ( IsShelleyBasedEra era
  , L.ShelleyEraTxCert (ShelleyLedgerEra era)
  )

class LegacyCertConstraints era => LegacyCertsC era

instance LegacyCertConstraints era => LegacyCertsC era

-- * Showcase functions

--
-- Functions that do not dispatch state their requirements once, in the
-- signature; 'withRange' appears only at the boundary (in the properties
-- below, where enumerated witnesses meet these signatures). Functions that
-- DO dispatch ('describeCert', 'eraGeneration') need 'withRange' inside the
-- branches: each branch's constraints do not hold for the whole input
-- range, so no signature could carry them.
--
-- The type applications on the ledger functions are required: @TxCert@ and
-- @ShelleyLedgerEra@ are type families, so the ledger era cannot be
-- inferred from the context. Call sites pin @era@ the same way.

-- | Era lower bound: 'L.ConwayEraTxCert' holds for every era from Conway
-- onwards. Dijkstra has its own concrete certificate type (@DijkstraTxCert@),
-- but still supports the class, so this function covers it too. The bundle
-- combines an api-level constraint with two independent ledger hierarchies
-- (certificates and outputs); @Show@ for the results needs no separate
-- constraint, as it is a superclass of both.
conwayDelegationCert
  :: forall era
   . ConwayCertConstraints era
  => L.Addr
  -> L.Credential L.Staking
  -> L.Delegatee
  -> String
conwayDelegationCert changeAddress credential delegatee =
  show (shelleyBasedEra @era)
    <> ": "
    <> show (L.mkDelegTxCert @(ShelleyLedgerEra era) credential delegatee)
    <> " paying to "
    <> show (L.mkCoinTxOut @(ShelleyLedgerEra era) changeAddress (L.Coin 0))

-- | Era upper bound: 'L.ShelleyEraTxCert' stops at Conway (the class itself
-- carries @AtMostEra \"Conway\"@), so it is only derivable for ranges bounded
-- above by Conway.
legacyRegistrationCert
  :: forall era
   . LegacyCertConstraints era
  => L.Credential L.Staking
  -> String
legacyRegistrationCert credential =
  show (shelleyBasedEra @era)
    <> ": "
    <> show (L.mkRegTxCert @(ShelleyLedgerEra era) credential)

-- | Era range subset: narrow the full timeline at the Babbage\/Conway
-- boundary with 'splitRange', gaining the Conway certificate constraints
-- only on the narrowed side. This is the total replacement for
-- @caseShelleyToBabbageOrConwayEraOnwards@. Here 'withRange' must sit inside
-- the branches: neither branch's constraints hold for the whole input range.
describeCert
  :: forall era
   . EraIn ShelleyBasedEras era
  -> L.Credential L.Staking
  -> L.Delegatee
  -> String
describeCert w credential delegatee =
  case splitRange @(ShelleyEra :-: BabbageEra) @(Onwards ConwayEra) w of
    Left shelleyToBabbage ->
      withRange @LegacyCertsC shelleyToBabbage $
        "legacy: " <> show (L.mkRegTxCert @(ShelleyLedgerEra era) credential)
    Right conwayOnwards ->
      withRange @ConwayCertsC conwayOnwards $
        "conway: " <> show (L.mkDelegTxCert @(ShelleyLedgerEra era) credential delegatee)

-- | Nested 'splitRange': the piece of one split is a range like any other,
-- so it can be split again, giving a total n-way dispatch. (The old
-- @caseShelleyToXOrYEraOnwards@ dispatchers could not be nested safely.)
eraGeneration :: EraIn ShelleyBasedEras era -> String
eraGeneration w =
  case splitRange @(ShelleyEra :-: MaryEra) @(Onwards AlonzoEra) w of
    Left _preAlonzo -> "pre-Plutus"
    Right alonzoOnwards ->
      case splitRange @(AlonzoEra :-: BabbageEra) @(Onwards ConwayEra) alonzoOnwards of
        Left _alonzoToBabbage -> "Plutus"
        Right _conwayOnwards -> "governance"

-- | Pattern matching a single era out of a range, idiom 1: case on the
-- 'CardanoEra' singleton. Each arm refines @era@ to the concrete era (so
-- every ledger instance resolves there), but the match cannot be exhaustive
-- for the range - the singleton type admits all eras, so a wildcard arm is
-- required for the eras the witness rules out.
nameEra :: EraIn (Onwards ConwayEra) era -> String
nameEra w =
  case eraInToCardanoEra w of
    ConwayEra -> "mainnet"
    DijkstraEra -> "upcoming"
    -- Not reliable, avoid: the checker cannot see that the witness rules
    -- these eras out, and the wildcard silently swallows any era later
    -- added to the range, losing the compile-time tripwire. Prefer the
    -- exhaustive 'onEra' chain ('nameEraExhaustive' below).
    _ -> "unreachable: not in Onwards ConwayEra"

-- | Pattern matching a single era out of a range, idiom 2: an exhaustive
-- 'onEra' chain over the existential form. This is total for exactly the
-- range - adding an era to the range makes this a compile error until a
-- handler is added - at the cost of erasing the type-level @era@.
nameEraExhaustive :: EraIn (Onwards ConwayEra) era -> String
nameEraExhaustive w =
  injectEra w
    & ( onEra @ConwayEra "mainnet"
          . onEra @DijkstraEra "upcoming"
          $ noMoreEras
      )

-- The negative space matters as much: these must NOT compile.
--
-- ShelleyEraTxCert has no Dijkstra instance, so requesting it for an
-- unbounded range fails with "No instance for ShelleyEraTxCert DijkstraEra":
--
-- > bad w = withRange @LegacyCertsC
-- >           (w :: EraIn (Onwards ConwayEra) era)
--
-- And an era outside the timeline is rejected by the range constructors:
--
-- > type Bad = Onwards Int   -- "From: era Int is not in the timeline"

-- * Properties

prop_era_range_lower_bound :: Property
prop_era_range_lower_bound = H.propertyOnce $ do
  changeAddress <- mkDummyAddress
  credential <- mkDummyStakeCredential
  delegatee <- L.DelegStake <$> mkDummyKeyHash
  -- The explicit signature is needed by GHC 9.6/9.10: GADTs implies
  -- MonoLocalBinds, so the let binding is not generalised, and the
  -- Foldable-generic consumers below leave the type ambiguous otherwise.
  let labels :: [String]
      labels =
        [ withRange @ConwayCertsC w $ conwayDelegationCert @era changeAddress credential delegatee
        | Some (w :: EraIn (Onwards ConwayEra) era) <- knownEras @(Onwards ConwayEra)
        ]
  length labels === 2 -- Conway and Dijkstra
  H.assertWith labels $ not . any null

prop_era_range_upper_bound :: Property
prop_era_range_upper_bound = H.propertyOnce $ do
  credential <- mkDummyStakeCredential
  let labels :: [String]
      labels =
        [ withRange @LegacyCertsC w $ legacyRegistrationCert @era credential
        | Some (w :: EraIn (ShelleyEra :-: ConwayEra) era) <- knownEras @(ShelleyEra :-: ConwayEra)
        ]
  length labels === 6 -- Shelley up to and including Conway
  H.assertWith labels $ not . any null

prop_era_range_subset :: Property
prop_era_range_subset = H.propertyOnce $ do
  credential <- mkDummyStakeCredential
  delegatee <- L.DelegStake <$> mkDummyKeyHash
  let labels =
        [ describeCert w credential delegatee
        | Some w <- knownEras @ShelleyBasedEras
        ]
      (legacy, conway) = partition ("legacy: " `isPrefixOf`) labels
  length legacy === 5 -- Shelley to Babbage
  length conway === 2 -- Conway and Dijkstra
  H.assertWith conway $ all ("conway: " `isPrefixOf`)

prop_era_range_nested_split :: Property
prop_era_range_nested_split = H.propertyOnce $ do
  let generations = [eraGeneration w | Some w <- knownEras @ShelleyBasedEras]
  generations
    === [ "pre-Plutus" -- Shelley
        , "pre-Plutus" -- Allegra
        , "pre-Plutus" -- Mary
        , "Plutus" -- Alonzo
        , "Plutus" -- Babbage
        , "governance" -- Conway
        , "governance" -- Dijkstra
        ]

prop_era_range_single_era_match :: Property
prop_era_range_single_era_match = H.propertyOnce $ do
  let names = [nameEra w | Some w <- knownEras @(Onwards ConwayEra)]
      namesExhaustive = [nameEraExhaustive w | Some w <- knownEras @(Onwards ConwayEra)]
      someEras = [injectEra w | Some w <- knownEras @(Onwards ConwayEra)]
  names === ["mainnet", "upcoming"]
  namesExhaustive === names
  map (isEra @ConwayEra) someEras === [True, False]
  map (isEra @DijkstraEra) someEras === [False, True]

prop_era_range_membership :: Property
prop_era_range_membership = H.propertyOnce $ do
  -- Babbage predates the Conway-onwards range.
  H.assertWith (checkMember @(Onwards ConwayEra) BabbageEra) isNothing
  -- Dijkstra is in every Onwards range (regression test for the missing
  -- Dijkstra arm in TestEquality CardanoEra).
  H.assertWith (checkMember @(Onwards ConwayEra) DijkstraEra) isJust
  H.assertWith (checkMember @ShelleyBasedEras DijkstraEra) isJust
  -- Dijkstra is beyond an upper bounded range.
  H.assertWith (checkMember @(ShelleyEra :-: ConwayEra) DijkstraEra) isNothing

prop_era_range_vary_roundtrip :: Property
prop_era_range_vary_roundtrip = H.propertyOnce $
  forM_ (knownEras @ShelleyBasedEras) $ \(Some w) ->
    withSomeEra (injectEra w) $ \w' ->
      AnyCardanoEra (eraInToCardanoEra w') === AnyCardanoEra (eraInToCardanoEra w)

-- * Dummy values

mkDummyKeyHash :: MonadTest m => m (L.KeyHash r)
mkDummyKeyHash = do
  hash <- H.nothingFail $ Hash.hashFromBytes $ BS.replicate 28 0
  pure $ L.KeyHash hash

mkDummyStakeCredential :: MonadTest m => m (L.Credential L.Staking)
mkDummyStakeCredential = L.KeyHashObj <$> mkDummyKeyHash

mkDummyAddress :: MonadTest m => m L.Addr
mkDummyAddress = do
  paymentKeyHash <- mkDummyKeyHash
  pure $ L.Addr L.Testnet (L.KeyHashObj paymentKeyHash) L.StakeRefNull

tests :: TestTree
tests =
  testGroup
    "Test.Cardano.Api.Experimental.EraRange"
    [ testProperty "era range lower bound" prop_era_range_lower_bound
    , testProperty "era range upper bound" prop_era_range_upper_bound
    , testProperty "era range subset via splitRange" prop_era_range_subset
    , testProperty "era range nested splitRange" prop_era_range_nested_split
    , testProperty "era range single era match" prop_era_range_single_era_match
    , testProperty "era range membership" prop_era_range_membership
    , testProperty "era range vary roundtrip" prop_era_range_vary_roundtrip
    ]
