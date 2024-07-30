{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.Api.Experimental.Script where

import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Protocol.AvailableEras
import           Cardano.Api.SerialiseCBOR (SerialiseAsCBOR (..))
import           Cardano.Api.TxIn

import qualified Cardano.Binary as CBOR
import qualified Cardano.Ledger.Allegra.Scripts as Ledger
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import qualified Cardano.Ledger.Alonzo.TxBody as Ledger
import qualified Cardano.Ledger.Alonzo.TxWits as Ledger
import qualified Cardano.Ledger.Babbage as Ledger
import           Cardano.Ledger.Binary
import qualified Cardano.Ledger.Binary.Plain as Plain
import qualified Cardano.Ledger.Conway as Ledger
import qualified Cardano.Ledger.Conway.Governance as Ledger
import qualified Cardano.Ledger.Conway.Scripts as Ledger
import qualified Cardano.Ledger.Core as Ledger
import           Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Plutus.Data as Ledger
import qualified Cardano.Ledger.TxIn as Ledger

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe.Strict
import           Data.Set (Set)
import           Data.Typeable
import           Data.Word
import           Lens.Micro

{-
In the current api we have PlutusScript which is a wrapper around ShortByteString
We should instead use newtype Plutus from ledger which is the same thing but
there is exposed functionality.
There is mkPlutusScript which will give us the different available scripts
for a given era (if they are available)

We are going to start with the assumption that the user will know what
script version they are using and will indicate as such.

I.e we will be removing ScriptInAnyLang because it depends on
the text envelope format and doesn't actually check the
script bytes.

We have an issue regarding what is and isn't available in the various eras

We can actually stick to the latest era and the upcoming era. We depend
on ledger to get the available script versions correctly so we should
be able to avoid parameterizing on versions.

-}

-- | This type wraps the serialized scripts (native or plutus)
-- for a given era. However we want to restrict the era to the
-- latest era (mainnet) and the upcoming era. This removes
-- the complexity of deciding which scripts are available in which eras.
newtype Script availableera
  = Script {unScript :: Ledger.Script (ToConstrainedEra availableera)}

instance
  ( HasTypeProxy (Script availableera)
  , ToCBOR (Ledger.Script (ToConstrainedEra availableera))
  , Typeable availableera
  , ConstrainedDecoder availableera ledgerera
  , Ledger.Era ledgerera
  , DecCBOR (Ledger.Script (ToConstrainedEra availableera))
  )
  => SerialiseAsCBOR (Script availableera)
  where
  serialiseToCBOR (Script s) = CBOR.serialize' s

  deserialiseFromCBOR _ bs =
    Plain.decodeFullDecoder
      "Script"
      fromCBOR
      (LBS.fromStrict bs)
      :: Either DecoderError (Script availableera)

instance HasTypeProxy (Script BabbageEra) where
  data AsType (Script BabbageEra) = AsMainnetScript
  proxyToAsType :: Proxy (Script BabbageEra) -> AsType (Script BabbageEra)
  proxyToAsType _ = AsMainnetScript

instance HasTypeProxy (Script ConwayEra) where
  data AsType (Script ConwayEra) = AsUpcomingEraScript
  proxyToAsType :: Proxy (Script ConwayEra) -> AsType (Script ConwayEra)
  proxyToAsType _ = AsUpcomingEraScript

instance
  ( Typeable availableera
  , Ledger.Era ledgerera
  , DecCBOR (Ledger.Script (ToConstrainedEra availableera))
  , ConstrainedDecoder availableera ledgerera
  )
  => FromCBOR (Script availableera)
  where
  fromCBOR = Script <$> fromEraCBORConstrained @availableera

-- The following serialization functions depend on the ledger's CBOR serialization
-- of Plutus scripts. Taking a Plutus ShortByteString and deserializing it to
-- the ledger's types will allow us to leverage the ledger's plutus type classes.
--

data NativeScriptDeserializationError
  = NotAScript DecoderError
  | NotASimpleScript -- We can improve this and potentially
  -- tell the consumer its a plutus script
  -- and which version it is.

deserialiseNativeScript
  :: DecCBOR (Ledger.AlonzoScript (ToConstrainedEra availableera))
  => Era availableera
  -> ByteString
  -> Either NativeScriptDeserializationError (Ledger.NativeScript (ToConstrainedEra availableera))
deserialiseNativeScript availableEra bs =
  case availableEra of
    CurrentEra -> deserialise AsMainnetScript bs
    UpcomingEra -> deserialise AsUpcomingEraScript bs
 where
  deserialise
    :: SerialiseAsCBOR (Script availableera)
    => Ledger.EraScript (ToConstrainedEra availableera)
    => AsType (Script availableera)
    -> ByteString
    -> Either NativeScriptDeserializationError (Ledger.NativeScript (ToConstrainedEra availableera))
  deserialise as bs' =
    case deserialiseFromCBOR as bs' of
      Right s -> case Ledger.getNativeScript $ unScript s of
        Just nScript -> Right nScript
        Nothing -> Left NotASimpleScript
      Left e -> Left $ NotAScript e

data PlutusScriptDeserializationError
  = NotAPlutusScript
  | NotAnyScript DecoderError

deserialisePlutusScript
  :: DecCBOR (Ledger.AlonzoScript (ToConstrainedEra availableera))
  => Era availableera
  -> ByteString
  -> Either PlutusScriptDeserializationError (Ledger.PlutusScript (ToConstrainedEra availableera))
deserialisePlutusScript era bs =
  case era of
    CurrentEra -> deserialise AsMainnetScript bs
    UpcomingEra -> deserialise AsUpcomingEraScript bs
 where
  deserialise
    :: SerialiseAsCBOR (Script availableera)
    => Ledger.AlonzoEraScript (ToConstrainedEra availableera)
    => AsType (Script availableera)
    -> ByteString
    -> Either PlutusScriptDeserializationError (Ledger.PlutusScript (ToConstrainedEra availableera))
  deserialise as bs' =
    case deserialiseFromCBOR as bs' of
      Right s -> case Ledger.toPlutusScript $ unScript s of
        Just nScript -> Right nScript
        Nothing -> Left NotAPlutusScript
      Left e -> Left $ NotAnyScript e

newtype ReferenceTxInput era
  = ReferenceTxInput {unReferenceTxInput :: TxIn}
  deriving Eq

data PlutusScriptWitness era
  = PlutusScriptWitness
      (Ledger.TxWits (ToConstrainedEra era))
  | PlutusScriptWitnessRefInput
      (Ledger.TxWits (ToConstrainedEra era))
      (ReferenceTxInput (ToConstrainedEra era))

deriving instance Eq (Ledger.TxWits (ToConstrainedEra era)) => Eq (PlutusScriptWitness era)

data SimpleScriptWitness era
  = SimpleScriptWitness
      (Ledger.TxWits (ToConstrainedEra era))
  | SimpleScriptWitnessRefInput
      (ReferenceTxInput (ToConstrainedEra era))

deriving instance Eq (Ledger.TxWits (ToConstrainedEra era)) => Eq (SimpleScriptWitness era)

createPlutusScriptWitness
  :: Ledger.Script (ToConstrainedEra era) ~ Ledger.AlonzoScript (ToConstrainedEra era)
  => Ledger.AlonzoEraScript (ToConstrainedEra era)
  => Era era
  -> Ledger.PlutusScript (ToConstrainedEra era)
  -> Maybe (Ledger.BinaryData (ToConstrainedEra era))
  -- ^ Datum
  -> Map
      (Ledger.PlutusPurpose Ledger.AsIx (ToConstrainedEra era))
      (Ledger.Data (ToConstrainedEra era), Ledger.ExUnits)
  -- ^ Redeemers
  -> PlutusScriptWitness era
createPlutusScriptWitness era plutusScript mTxDatum redeemerMap =
  case era of
    CurrentEra -> PlutusScriptWitness $ createScriptWit $ Ledger.TxDats $ createDatumHashMap @BabbageEra mTxDatum
    UpcomingEra -> PlutusScriptWitness $ createScriptWit $ Ledger.TxDats $ createDatumHashMap @ConwayEra mTxDatum
 where
  createScriptWit datumHashMap =
    let script = Ledger.PlutusScript plutusScript
        scriptHashMap = Map.singleton (Ledger.hashScript script) script
     in Ledger.AlonzoTxWits mempty mempty scriptHashMap datumHashMap $ Ledger.Redeemers redeemerMap

createDatumHashMap
  :: Ledger.AlonzoEraScript (ToConstrainedEra era)
  => Maybe (Ledger.BinaryData (ToConstrainedEra era))
  -> Map (Ledger.DataHash (Ledger.EraCrypto (ToConstrainedEra era))) (Ledger.Data (ToConstrainedEra era))
createDatumHashMap =
  maybe
    mempty
    (\binaryData -> Map.singleton (Ledger.hashBinaryData binaryData) (Ledger.binaryDataToData binaryData))

createPlutusReferenceScriptWitness
  :: Ledger.Script (ToConstrainedEra era) ~ Ledger.AlonzoScript (ToConstrainedEra era)
  => Ledger.AlonzoEraScript (ToConstrainedEra era)
  => Era era
  -> ReferenceTxInput (ToConstrainedEra era)
  -> Maybe (Ledger.BinaryData (ToConstrainedEra era))
  -- ^ Datum
  -> Map
      (Ledger.PlutusPurpose Ledger.AsIx (ToConstrainedEra era))
      (Ledger.Data (ToConstrainedEra era), Ledger.ExUnits)
  -- ^ Redeemers
  -> PlutusScriptWitness era
createPlutusReferenceScriptWitness era txin mTxDatum redeemerMap =
  case era of
    CurrentEra ->
      PlutusScriptWitnessRefInput
        (createScriptWit (Ledger.TxDats $ createDatumHashMap @BabbageEra mTxDatum))
        txin
    UpcomingEra ->
      PlutusScriptWitnessRefInput
        (createScriptWit (Ledger.TxDats $ createDatumHashMap @ConwayEra mTxDatum))
        txin
 where
  createScriptWit datumHashMap =
    Ledger.AlonzoTxWits mempty mempty mempty datumHashMap $ Ledger.Redeemers redeemerMap

createSimpleScriptWitness
  :: Ledger.AlonzoEraScript (ToConstrainedEra era)
  => Ledger.Script (ToConstrainedEra era) ~ Ledger.AlonzoScript (ToConstrainedEra era)
  => Ledger.NativeScript (ToConstrainedEra era) ~ Ledger.Timelock (ToConstrainedEra era)
  => Era era
  -> Ledger.NativeScript (ToConstrainedEra era)
  -> SimpleScriptWitness era
createSimpleScriptWitness era simpleScript = case era of
  CurrentEra -> SimpleScriptWitness createScriptWit
  UpcomingEra -> SimpleScriptWitness createScriptWit
 where
  createScriptWit =
    let script = Ledger.TimelockScript simpleScript
        scriptHashMap = Map.singleton (Ledger.hashScript script) script
     in Ledger.AlonzoTxWits mempty mempty scriptHashMap mempty (Ledger.Redeemers mempty)

createSimpleReferenceScriptWitness
  :: Era era
  -> ReferenceTxInput (ToConstrainedEra era)
  -> SimpleScriptWitness era
createSimpleReferenceScriptWitness era refInput = case era of
  CurrentEra -> SimpleScriptWitnessRefInput refInput
  UpcomingEra -> SimpleScriptWitnessRefInput refInput

data RedeemerConstructionError item container
  = ItemToBeWitnessedNotFound item container

createTxInRedeemer
  :: Ledger.AlonzoEraScript (ToConstrainedEra era)
  => Ledger.EraCrypto (ToConstrainedEra era) ~ StandardCrypto
  => Era era
  -> Ledger.TxIn StandardCrypto
  -- ^ Input to be witnessed by
  -> Set (Ledger.TxIn StandardCrypto)
  -> Ledger.BinaryData (ToConstrainedEra era)
  -> Ledger.ExUnits
  -> Either
      (RedeemerConstructionError (Ledger.TxIn StandardCrypto) (Set (Ledger.TxIn StandardCrypto)))
      (SingleRedeemer era)
createTxInRedeemer era toBeWitnessed allTxInputs =
  createSingleRedeemerMapEntry era toBeWitnessed allTxInputs Ledger.mkSpendingPurpose

createVotingRedeemer
  :: Ledger.Voter StandardCrypto
  -- ^ Input to be witnessed by
  -> Set (Ledger.Voter StandardCrypto)
  -> Ledger.BinaryData (ToConstrainedEra ConwayEra)
  -> Ledger.ExUnits
  -> Either
      (RedeemerConstructionError (Ledger.Voter StandardCrypto) (Set (Ledger.Voter StandardCrypto)))
      (SingleRedeemer ConwayEra)
createVotingRedeemer toBeWitnessed allTxInputs =
  createSingleRedeemerMapEntry UpcomingEra toBeWitnessed allTxInputs Ledger.mkVotingPurpose

type SingleRedeemer era =
  ( Map
      (Ledger.PlutusPurpose Ledger.AsIx (ToConstrainedEra era))
      (Ledger.Data (ToConstrainedEra era), Ledger.ExUnits)
  )

-- Helper functions
-- TODO: It would be useful to have a type class that
-- maps era -> [Language] i.e allowed script languages in a given era
createSingleRedeemerMapEntry
  :: forall item container era
   . Ledger.Indexable item container
  => Ledger.Era (ToConstrainedEra era)
  => Era era
  -> item
  -- ^ Item to be witnessed (TxIn, Cert, etc)
  -> container
  -- ^ All items in transaction
  -> (Ledger.AsIx Word32 item -> Ledger.PlutusPurpose Ledger.AsIx (ToConstrainedEra era))
  -> Ledger.BinaryData (ToConstrainedEra era)
  -> Ledger.ExUnits
  -> Either (RedeemerConstructionError item container) (SingleRedeemer era)
createSingleRedeemerMapEntry era toBeWitnessed allThings toPlutusPurpose redeemerBinaryData exunits =
  case era of
    CurrentEra -> createRedeemer
    UpcomingEra -> createRedeemer
 where
  createRedeemer
    :: Either (RedeemerConstructionError item container) (SingleRedeemer era)
  createRedeemer = do
    let asItem = Ledger.AsItem toBeWitnessed

    index <-
      maybe
        (Left $ ItemToBeWitnessedNotFound toBeWitnessed allThings)
        Right
        $ strictMaybeToMaybe
        $ Ledger.indexOf asItem allThings

    let plutusPurpose = toPlutusPurpose index
        redeemerData = Ledger.binaryDataToData redeemerBinaryData

    return $ Map.singleton plutusPurpose (redeemerData, exunits)

getAllPlutusScripts
  :: Ledger.EraTxWits (ToConstrainedEra era)
  => [PlutusScriptWitness era]
  -> [Ledger.Script (ToConstrainedEra era)]
getAllPlutusScripts [] = []
getAllPlutusScripts plutusScriptWits =
  mconcat
    [ Map.elems $ txWits ^. Ledger.scriptTxWitsL
    | PlutusScriptWitness txWits <- plutusScriptWits
    ]

getAllSimpleScripts
  :: Ledger.EraTxWits (ToConstrainedEra era)
  => [SimpleScriptWitness era]
  -> [Ledger.Script (ToConstrainedEra era)]
getAllSimpleScripts [] = []
getAllSimpleScripts simpleScriptWits =
  mconcat
    [ Map.elems $ txWits ^. Ledger.scriptTxWitsL
    | SimpleScriptWitness txWits <- simpleScriptWits
    ]

class ConstrainedDecoder availableera era | availableera -> era where
  fromEraCBORConstrained :: (Ledger.Era era, DecCBOR t) => Plain.Decoder s t

instance ConstrainedDecoder BabbageEra Ledger.Babbage where
  fromEraCBORConstrained = Ledger.fromEraCBOR @Ledger.Babbage

instance ConstrainedDecoder ConwayEra Ledger.Conway where
  fromEraCBORConstrained = Ledger.fromEraCBOR @Ledger.Conway
