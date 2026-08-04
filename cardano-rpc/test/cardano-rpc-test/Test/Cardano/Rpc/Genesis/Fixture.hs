{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Differential tests: each genesis-to-proto mapper
-- ("Cardano.Rpc.Server.Internal.UtxoRpc.Type.Genesis") runs on a real,
-- trimmed mainnet genesis file, and the output is compared against the
-- file's own text.
--
-- Why fixtures exist on top of "Test.Cardano.Rpc.Genesis.Property": a
-- property test computes its expected values with formulas that mirror the
-- mapper. When both sides share a wrong assumption - wrong base64 alphabet,
-- wrong denominator - they agree, and the test passes over broken output.
-- The fixture file is an oracle the implementation cannot contaminate.
-- Its text is what cardano-node boots from and what other UTxO RPC
-- consumers read, so the assertion "this proto field equals the bytes in
-- the file" holds only when the encoding is genuinely right.
--
-- The end-to-end test cannot cover this either: a test network's genesis
-- has the interesting fields empty (AVVM balances, heavy delegation,
-- genesis delegates, initial funds), so those encodings never reach the
-- wire there.
--
-- Each test does three things:
--
-- 1. Parse the fixture with the era's own ledger parser, the same one
--    cardano-node uses at boot (canonical JSON for Byron, aeson for the
--    rest).
--
-- 2. Run the parsed value through the era's @*GenesisToProto@ mapper.
--
-- 3. Re-parse the same fixture as a plain 'Aeson.Value' and assert the
--    proto fields against the raw JSON text.
--
-- Intentional divergences from the file's text are pinned with explicit
-- literals and a comment. Example: @slotLength@ is seconds in the JSON but
-- milliseconds in the proto.
module Test.Cardano.Rpc.Genesis.Fixture where

import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.Genesis

import Cardano.Chain.Genesis qualified as Byron (GenesisData, readGenesisData)
import Cardano.Ledger.Alonzo.Genesis qualified as L (AlonzoGenesis)
import Cardano.Ledger.Conway.Genesis qualified as L (ConwayGenesis)
import Cardano.Ledger.Plutus qualified as L (Language (PlutusV1), costModelParamNames)
import Cardano.Ledger.Shelley.Genesis qualified as L (ShelleyGenesis)

import RIO

import Control.Monad.Except (runExceptT)
import Data.Aeson (FromJSON, Value (..), (.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson (parseEither)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Map.Strict qualified as Map
import Data.ProtoLens (defMessage)
import Data.Ratio (denominator, numerator)
import Data.Scientific (Scientific)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text (encodeUtf8)
import Network.GRPC.Spec

import Hedgehog
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H

--------------------------------------------------------------------------------
-- Fixture paths
--------------------------------------------------------------------------------

byronFixturePath, shelleyFixturePath, alonzoFixturePath, conwayFixturePath :: FilePath
byronFixturePath = "test/cardano-rpc-test/files/genesis/byron-genesis.json"
shelleyFixturePath = "test/cardano-rpc-test/files/genesis/shelley-genesis.json"
alonzoFixturePath = "test/cardano-rpc-test/files/genesis/alonzo-genesis.json"
conwayFixturePath = "test/cardano-rpc-test/files/genesis/conway-genesis.json"

--------------------------------------------------------------------------------
-- Byron
--------------------------------------------------------------------------------

-- | Byron parses via canonical JSON ('Byron.readGenesisData'), not aeson, so
-- this is the only fixture read with two different parsers on purpose.
hprop_byron_genesis_fixture :: Property
hprop_byron_genesis_fixture = H.propertyOnce $ do
  genesisData <- readByronGenesisData byronFixturePath
  rawObj <- decodeFixtureObject byronFixturePath

  let message = byronGenesisToProto genesisData defMessage

  -- avvmDistr: base64url AVVM redeem key -> lovelace amount, both verbatim.
  expectedAvvmDistr <- field @(Map Text Text) rawObj "avvmDistr"
  message ^. U5c.avvmDistr === expectedAvvmDistr

  -- nonAvvmBalances: base58 address -> lovelace amount, both verbatim.
  -- (Empty on mainnet; this fixture's two entries are fabricated from real
  -- addresses so 'decodeAddressBase58' accepts them.)
  expectedNonAvvmBalances <- field @(Map Text Text) rawObj "nonAvvmBalances"
  message ^. U5c.nonAvvmBalances === expectedNonAvvmBalances

  -- bootStakeholders: the genesis file carries only a key *set*; the ledger
  -- synthesises weight 1 for each when it re-serialises, which is what the
  -- mapper also does.
  bootStakeholdersRaw <- field @(Map Text Integer) rawObj "bootStakeholders"
  let actualBootStakeholders = message ^. U5c.bootStakeholders
  Map.keysSet actualBootStakeholders === Map.keysSet bootStakeholdersRaw
  H.assertWith actualBootStakeholders (all (== 1) . Map.elems)

  -- heavyDelegation: issuer key hash hex -> certificate fields, all verbatim.
  heavyDelegationRaw <- subObjects rawObj "heavyDelegation"
  expectedHeavyDelegation <-
    traverse
      ( \certObj ->
          (,,,)
            <$> field @Text certObj "cert"
            <*> field @Text certObj "delegatePk"
            <*> field @Text certObj "issuerPk"
            <*> field @Integer certObj "omega"
      )
      heavyDelegationRaw
  let actualHeavyDelegation =
        Map.map
          ( \cert ->
              ( cert ^. U5c.cert
              , cert ^. U5c.delegatePk
              , cert ^. U5c.issuerPk
              , fromIntegral (cert ^. U5c.omega)
              )
          )
          (message ^. U5c.heavyDelegation)
  actualHeavyDelegation === expectedHeavyDelegation

  -- protocolConsts
  protocolConstsRaw <- subObject rawObj "protocolConsts"
  expectedK <- field @Integer protocolConstsRaw "k"
  expectedProtocolMagic <- field @Integer protocolConstsRaw "protocolMagic"
  fromIntegral (message ^. U5c.protocolConsts . U5c.k) === expectedK
  fromIntegral (message ^. U5c.protocolConsts . U5c.protocolMagic) === expectedProtocolMagic

  -- startTime: Unix seconds, straight passthrough.
  expectedStartTime <- field @Integer rawObj "startTime"
  fromIntegral (message ^. U5c.startTime) === expectedStartTime

  -- blockVersionData: every threshold and size, byte-for-byte against the
  -- genesis JSON's decimal-string numerals.
  blockVersionDataRaw <- subObject rawObj "blockVersionData"
  let actualBlockVersionData = message ^. U5c.blockVersionData

  expectedScriptVersion <- field @Integer blockVersionDataRaw "scriptVersion"
  fromIntegral (actualBlockVersionData ^. U5c.scriptVersion) === expectedScriptVersion

  for_
    [ (U5c.slotDuration, "slotDuration")
    , (U5c.maxBlockSize, "maxBlockSize")
    , (U5c.maxHeaderSize, "maxHeaderSize")
    , (U5c.maxTxSize, "maxTxSize")
    , (U5c.maxProposalSize, "maxProposalSize")
    , (U5c.mpcThd, "mpcThd")
    , (U5c.heavyDelThd, "heavyDelThd")
    , (U5c.updateVoteThd, "updateVoteThd")
    , (U5c.updateProposalThd, "updateProposalThd")
    , (U5c.updateImplicit, "updateImplicit")
    , (U5c.unlockStakeEpoch, "unlockStakeEpoch")
    ]
    $ \(getField, key) -> do
      expected <- field @Text blockVersionDataRaw key
      actualBlockVersionData ^. getField === expected

  softforkRuleRaw <- subObject blockVersionDataRaw "softforkRule"
  for_
    [ (U5c.initThd, "initThd")
    , (U5c.minThd, "minThd")
    , (U5c.thdDecrement, "thdDecrement")
    ]
    $ \(getField, key) -> do
      expected <- field @Text softforkRuleRaw key
      actualBlockVersionData ^. (U5c.softforkRule . getField) === expected

  txFeePolicyRaw <- subObject blockVersionDataRaw "txFeePolicy"
  expectedSummand <- field @Text txFeePolicyRaw "summand"
  expectedMultiplier <- field @Text txFeePolicyRaw "multiplier"
  actualBlockVersionData ^. (U5c.txFeePolicy . U5c.summand) === expectedSummand
  actualBlockVersionData ^. (U5c.txFeePolicy . U5c.multiplier) === expectedMultiplier

readByronGenesisData :: (MonadTest m, MonadIO m) => FilePath -> m Byron.GenesisData
readByronGenesisData path = do
  result <- H.evalIO $ runExceptT (Byron.readGenesisData path)
  (genesisData, _genesisHash) <- H.leftFail result
  pure genesisData

--------------------------------------------------------------------------------
-- Shelley
--------------------------------------------------------------------------------

hprop_shelley_genesis_fixture :: Property
hprop_shelley_genesis_fixture = H.propertyOnce $ do
  genesis <- decodeFixtureAs @L.ShelleyGenesis shelleyFixturePath
  rawObj <- decodeFixtureObject shelleyFixturePath

  let message = shelleyGenesisToProto genesis defMessage

  expectedEpochLength <- field @Integer rawObj "epochLength"
  fromIntegral (message ^. U5c.epochLength) === expectedEpochLength

  expectedMaxKesEvolutions <- field @Integer rawObj "maxKESEvolutions"
  fromIntegral (message ^. U5c.maxKesEvolutions) === expectedMaxKesEvolutions

  expectedSlotsPerKesPeriod <- field @Integer rawObj "slotsPerKESPeriod"
  fromIntegral (message ^. U5c.slotsPerKesPeriod) === expectedSlotsPerKesPeriod

  expectedUpdateQuorum <- field @Integer rawObj "updateQuorum"
  fromIntegral (message ^. U5c.updateQuorum) === expectedUpdateQuorum

  expectedSecurityParam <- field @Integer rawObj "securityParam"
  fromIntegral (message ^. U5c.securityParam) === expectedSecurityParam

  expectedNetworkMagic <- field @Integer rawObj "networkMagic"
  fromIntegral (message ^. U5c.networkMagic) === expectedNetworkMagic

  expectedNetworkId <- field @Text rawObj "networkId"
  message ^. U5c.networkId === expectedNetworkId

  expectedSystemStart <- field @Text rawObj "systemStart"
  message ^. U5c.systemStart === expectedSystemStart

  expectedMaxLovelaceSupply <- field @Integer rawObj "maxLovelaceSupply"
  fromIntegral (message ^. U5c.maxLovelaceSupply . U5c.int) === expectedMaxLovelaceSupply

  -- The genesis JSON's slotLength is in whole seconds; the proto field is
  -- milliseconds (see 'Cardano.Rpc.Server.Internal.UtxoRpc.Type.Genesis'),
  -- so the expected value is scaled rather than compared verbatim.
  expectedSlotLengthSeconds <- field @Integer rawObj "slotLength"
  fromIntegral (message ^. U5c.slotLength) === expectedSlotLengthSeconds * 1000

  -- genDelegs: genesis key hash hex -> (delegate hash, VRF hash), verbatim.
  genDelegsRaw <- subObjects rawObj "genDelegs"
  expectedGenDelegs <-
    traverse
      (\obj -> (,) <$> field @Text obj "delegate" <*> field @Text obj "vrf")
      genDelegsRaw
  let actualGenDelegs =
        Map.map (\gd -> (gd ^. U5c.delegate, gd ^. U5c.vrf)) (message ^. U5c.genDelegs)
  actualGenDelegs === expectedGenDelegs

  -- initialFunds: hex-serialised address -> lovelace amount, verbatim.
  -- (Empty on mainnet; fabricated here from a real testnet genesis so the
  -- addresses are valid.)
  expectedInitialFunds <- field @(Map Text Integer) rawObj "initialFunds"
  let actualInitialFunds =
        Map.map (\coin -> fromIntegral (coin ^. U5c.int)) (message ^. U5c.initialFunds)
  actualInitialFunds === expectedInitialFunds

--------------------------------------------------------------------------------
-- Alonzo
--------------------------------------------------------------------------------

hprop_alonzo_genesis_fixture :: Property
hprop_alonzo_genesis_fixture = H.propertyOnce $ do
  genesis <- decodeFixtureAs @L.AlonzoGenesis alonzoFixturePath
  rawObj <- decodeFixtureObject alonzoFixturePath

  let message = alonzoGenesisToProto genesis defMessage

  expectedLovelacePerUtxoWord <- field @Integer rawObj "lovelacePerUTxOWord"
  fromIntegral (message ^. U5c.lovelacePerUtxoWord . U5c.int) === expectedLovelacePerUtxoWord

  executionPricesRaw <- subObject rawObj "executionPrices"
  checkFractionField executionPricesRaw "prSteps" (message ^. U5c.executionPrices . U5c.steps)
  checkFractionField executionPricesRaw "prMem" (message ^. U5c.executionPrices . U5c.memory)

  maxTxExUnitsRaw <- subObject rawObj "maxTxExUnits"
  expectedMaxTxExMem <- field @Integer maxTxExUnitsRaw "exUnitsMem"
  expectedMaxTxExSteps <- field @Integer maxTxExUnitsRaw "exUnitsSteps"
  fromIntegral (message ^. U5c.maxTxExUnits . U5c.memory) === expectedMaxTxExMem
  fromIntegral (message ^. U5c.maxTxExUnits . U5c.steps) === expectedMaxTxExSteps

  maxBlockExUnitsRaw <- subObject rawObj "maxBlockExUnits"
  expectedMaxBlockExMem <- field @Integer maxBlockExUnitsRaw "exUnitsMem"
  expectedMaxBlockExSteps <- field @Integer maxBlockExUnitsRaw "exUnitsSteps"
  fromIntegral (message ^. U5c.maxBlockExUnits . U5c.memory) === expectedMaxBlockExMem
  fromIntegral (message ^. U5c.maxBlockExUnits . U5c.steps) === expectedMaxBlockExSteps

  expectedMaxValueSize <- field @Integer rawObj "maxValueSize"
  fromIntegral (message ^. U5c.maxValueSize) === expectedMaxValueSize

  expectedCollateralPercentage <- field @Integer rawObj "collateralPercentage"
  fromIntegral (message ^. U5c.collateralPercentage) === expectedCollateralPercentage

  expectedMaxCollateralInputs <- field @Integer rawObj "maxCollateralInputs"
  fromIntegral (message ^. U5c.maxCollateralInputs) === expectedMaxCollateralInputs

  -- PlutusV1 cost model: every named parameter, byte-for-byte against the
  -- genesis JSON object. 'L.costModelParamNames' recovers the names for the
  -- positional 'values' list the mapper writes (mirroring how the ledger's
  -- own 'costModelToMap' pairs them up).
  costModelsRaw <- subObject rawObj "costModels"
  expectedPlutusV1 <- field @(Map Text Int64) costModelsRaw "PlutusV1"
  let actualPlutusV1 =
        Map.fromList $
          zip
            (L.costModelParamNames L.PlutusV1)
            (message ^. U5c.costModels . U5c.plutusV1 . U5c.values)
  actualPlutusV1 === expectedPlutusV1
  -- Every fixture parameter is one the mapper actually emitted (guards
  -- against 'costModelParamNames' and the genesis JSON silently drifting
  -- apart in length, which 'Map.fromList' zipping would otherwise hide).
  Map.keysSet actualPlutusV1 === Map.keysSet expectedPlutusV1

--------------------------------------------------------------------------------
-- Conway
--------------------------------------------------------------------------------

hprop_conway_genesis_fixture :: Property
hprop_conway_genesis_fixture = H.propertyOnce $ do
  genesis <- decodeFixtureAs @L.ConwayGenesis conwayFixturePath
  rawObj <- decodeFixtureObject conwayFixturePath

  let message = conwayGenesisToProto genesis defMessage

  expectedCommitteeMinSize <- field @Integer rawObj "committeeMinSize"
  fromIntegral (message ^. U5c.committeeMinSize) === expectedCommitteeMinSize

  expectedCommitteeMaxTermLength <- field @Integer rawObj "committeeMaxTermLength"
  fromIntegral (message ^. U5c.committeeMaxTermLength) === expectedCommitteeMaxTermLength

  expectedGovActionLifetime <- field @Integer rawObj "govActionLifetime"
  fromIntegral (message ^. U5c.govActionLifetime) === expectedGovActionLifetime

  expectedDrepActivity <- field @Integer rawObj "dRepActivity"
  fromIntegral (message ^. U5c.drepActivity) === expectedDrepActivity

  expectedGovActionDeposit <- field @Integer rawObj "govActionDeposit"
  fromIntegral (message ^. U5c.govActionDeposit . U5c.int) === expectedGovActionDeposit

  expectedDrepDeposit <- field @Integer rawObj "dRepDeposit"
  fromIntegral (message ^. U5c.drepDeposit . U5c.int) === expectedDrepDeposit

  checkRatioField rawObj "minFeeRefScriptCostPerByte" (message ^. U5c.minFeeRefScriptCostPerByte)

  poolVotingThresholdsRaw <- subObject rawObj "poolVotingThresholds"
  let actualPoolVotingThresholds = message ^. U5c.poolVotingThresholds
  checkRatioField
    poolVotingThresholdsRaw
    "motionNoConfidence"
    (actualPoolVotingThresholds ^. U5c.motionNoConfidence)
  checkRatioField
    poolVotingThresholdsRaw
    "committeeNormal"
    (actualPoolVotingThresholds ^. U5c.committeeNormal)
  checkRatioField
    poolVotingThresholdsRaw
    "committeeNoConfidence"
    (actualPoolVotingThresholds ^. U5c.committeeNoConfidence)
  checkRatioField
    poolVotingThresholdsRaw
    "hardForkInitiation"
    (actualPoolVotingThresholds ^. U5c.hardForkInitiation)
  checkRatioField
    poolVotingThresholdsRaw
    "ppSecurityGroup"
    (actualPoolVotingThresholds ^. U5c.ppSecurityGroup)

  drepVotingThresholdsRaw <- subObject rawObj "dRepVotingThresholds"
  let actualDrepVotingThresholds = message ^. U5c.drepVotingThresholds
  checkRatioField
    drepVotingThresholdsRaw
    "motionNoConfidence"
    (actualDrepVotingThresholds ^. U5c.motionNoConfidence)
  checkRatioField
    drepVotingThresholdsRaw
    "committeeNormal"
    (actualDrepVotingThresholds ^. U5c.committeeNormal)
  checkRatioField
    drepVotingThresholdsRaw
    "committeeNoConfidence"
    (actualDrepVotingThresholds ^. U5c.committeeNoConfidence)
  checkRatioField
    drepVotingThresholdsRaw
    "updateToConstitution"
    (actualDrepVotingThresholds ^. U5c.updateToConstitution)
  checkRatioField
    drepVotingThresholdsRaw
    "hardForkInitiation"
    (actualDrepVotingThresholds ^. U5c.hardForkInitiation)
  checkRatioField
    drepVotingThresholdsRaw
    "ppNetworkGroup"
    (actualDrepVotingThresholds ^. U5c.ppNetworkGroup)
  checkRatioField
    drepVotingThresholdsRaw
    "ppEconomicGroup"
    (actualDrepVotingThresholds ^. U5c.ppEconomicGroup)
  checkRatioField
    drepVotingThresholdsRaw
    "ppTechnicalGroup"
    (actualDrepVotingThresholds ^. U5c.ppTechnicalGroup)
  checkRatioField drepVotingThresholdsRaw "ppGovGroup" (actualDrepVotingThresholds ^. U5c.ppGovGroup)
  checkRatioField
    drepVotingThresholdsRaw
    "treasuryWithdrawal"
    (actualDrepVotingThresholds ^. U5c.treasuryWithdrawal)

  -- PlutusV3 cost model: a plain positional array from Conway onwards (no
  -- named parameters in the genesis JSON, unlike Alonzo's PlutusV1), so this
  -- compares directly against the mapper's output list.
  expectedPlutusV3 <- field @[Int64] rawObj "plutusV3CostModel"
  message ^. U5c.costModels . U5c.plutusV3 . U5c.values === expectedPlutusV3

  -- Constitution: anchor URL/hash and the guardrails script hash. Anchor
  -- 'contentHash' and the constitution 'hash' are raw bytes on the proto
  -- side, hex text in the genesis JSON, so the expected value is hex-decoded
  -- before comparing.
  constitutionRaw <- subObject rawObj "constitution"
  anchorRaw <- subObject constitutionRaw "anchor"
  expectedAnchorUrl <- field @Text anchorRaw "url"
  expectedAnchorDataHashHex <- field @Text anchorRaw "dataHash"
  expectedScriptHashHex <- field @Text constitutionRaw "script"
  expectedAnchorDataHash <- hexBytes expectedAnchorDataHashHex
  expectedScriptHash <- hexBytes expectedScriptHashHex

  message ^. U5c.constitution . U5c.anchor . U5c.url === expectedAnchorUrl
  message ^. U5c.constitution . U5c.anchor . U5c.contentHash === expectedAnchorDataHash
  message ^. U5c.constitution . U5c.hash === expectedScriptHash

  -- Committee: threshold and, per member, the epoch bound. Genesis JSON
  -- credential keys are prefixed ("scriptHash-"/"keyHash-"); the mapper
  -- renders bare hex, so the prefix is stripped before comparing.
  --
  -- Unlike the vote thresholds above, the committee threshold is written as
  -- an explicit fraction object in the genesis JSON (not a decimal numeral),
  -- hence 'checkFractionField' rather than 'checkRatioField'.
  committeeRaw <- subObject rawObj "committee"
  checkFractionField committeeRaw "threshold" (message ^. U5c.committee . U5c.threshold)

  membersRaw <- field @(Map Text Integer) committeeRaw "members"
  let stripCredentialPrefix credential =
        fromMaybe credential $
          Text.stripPrefix "scriptHash-" credential <|> Text.stripPrefix "keyHash-" credential
      expectedMembers = Map.mapKeys stripCredentialPrefix membersRaw
      actualMembers = Map.map fromIntegral (message ^. U5c.committee . U5c.members)
  actualMembers === expectedMembers

--------------------------------------------------------------------------------
-- Aeson helpers
--------------------------------------------------------------------------------

decodeFixtureValue :: (MonadTest m, MonadIO m) => FilePath -> m Value
decodeFixtureValue path = do
  bytes <- H.evalIO $ BS.readFile path
  H.leftFail (Aeson.eitherDecodeStrict' bytes)

decodeFixtureObject :: (MonadTest m, MonadIO m) => FilePath -> m Aeson.Object
decodeFixtureObject path = decodeFixtureValue path >>= asObject

decodeFixtureAs :: forall a m. (FromJSON a, MonadTest m, MonadIO m) => FilePath -> m a
decodeFixtureAs path = do
  bytes <- H.evalIO $ BS.readFile path
  H.leftFail (Aeson.eitherDecodeStrict' bytes)

asObject :: MonadTest m => Value -> m Aeson.Object
asObject value = H.nothingFail $ case value of
  Object obj -> Just obj
  _ -> Nothing

field :: forall a m. (FromJSON a, MonadTest m) => Aeson.Object -> Aeson.Key -> m a
field obj key = H.leftFail (Aeson.parseEither (.: key) obj)

subObject :: MonadTest m => Aeson.Object -> Aeson.Key -> m Aeson.Object
subObject obj key = field @Value obj key >>= asObject

-- | Fetch an object-valued field as a map of its entries, each still an
-- object (e.g. Byron's @heavyDelegation@, Shelley's @genDelegs@ - maps keyed
-- by hash hex, whose values are themselves small objects).
subObjects :: MonadTest m => Aeson.Object -> Aeson.Key -> m (Map Text Aeson.Object)
subObjects obj key = do
  valueMap <- field @(Map Text Value) obj key
  traverse asObject valueMap

hexBytes :: MonadTest m => Text -> m ByteString
hexBytes hex = H.leftFail (Base16.decode (Text.encodeUtf8 hex))

-- | Assert a proto 'U5c.RationalNumber' field against the exact rational
-- value of the genesis JSON's decimal numeral for the same field (via
-- 'Scientific's exact 'Rational' conversion, so e.g. @0.51@ compares as
-- @51 % 100@, not a floating-point approximation).
--
-- Only for fields written as a plain decimal numeral in the genesis JSON
-- (e.g. Conway's vote thresholds). Fields written as an explicit
-- @{"numerator":.., "denominator":..}@ object (e.g. Alonzo's
-- @executionPrices@, Conway's committee @threshold@) need 'checkFractionField'
-- instead.
checkRatioField
  :: MonadTest m
  => Aeson.Object
  -> Aeson.Key
  -> Proto U5c.RationalNumber
  -> m ()
checkRatioField obj key actual = do
  scientificValue <- field @Scientific obj key
  let expected = toRational scientificValue
  fromIntegral (actual ^. U5c.numerator) === numerator expected
  fromIntegral (actual ^. U5c.denominator) === denominator expected

-- | Assert a proto 'U5c.RationalNumber' field against an explicit
-- @{"numerator":.., "denominator":..}@ object in the genesis JSON.
checkFractionField
  :: MonadTest m
  => Aeson.Object
  -> Aeson.Key
  -> Proto U5c.RationalNumber
  -> m ()
checkFractionField obj key actual = do
  fractionRaw <- subObject obj key
  expectedNumerator <- field @Integer fractionRaw "numerator"
  expectedDenominator <- field @Integer fractionRaw "denominator"
  fromIntegral (actual ^. U5c.numerator) === expectedNumerator
  fromIntegral (actual ^. U5c.denominator) === expectedDenominator
