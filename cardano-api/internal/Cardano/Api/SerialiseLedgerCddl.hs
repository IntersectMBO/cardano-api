{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Ledger CDDL Serialisation
--
module Cardano.Api.SerialiseLedgerCddl
  ( TextEnvelopeCddl(..)
  , TextEnvelopeCddlError (..)
  , FromSomeTypeCDDL(..)

  -- * Reading one of several transaction or
  -- key witness types
  , readFileTextEnvelopeCddlAnyOf
  , deserialiseFromTextEnvelopeCddlAnyOf

  , writeTxFileTextEnvelopeCddl
  , writeTxWitnessFileTextEnvelopeCddl

  -- Exported for testing
  , serialiseTxLedgerCddl
  , deserialiseTxLedgerCddl
  , deserialiseByronTxCddl
  , serialiseWitnessLedgerCddl
  , deserialiseWitnessLedgerCddl

    -- * Byron tx serialization
  , serializeByronTx
  , writeByronTxFileTextEnvelopeCddl
  )
  where

import           Cardano.Api.Eon.ShelleyBasedEra
import           Cardano.Api.Error
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.IO
import           Cardano.Api.Pretty
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.Tx
import           Cardano.Api.Utils

import qualified Cardano.Chain.UTxO as Byron
import           Cardano.Ledger.Binary (DecoderError)
import qualified Cardano.Ledger.Binary as CBOR

import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither,
                   newExceptT, runExceptT)
import           Data.Aeson
import qualified Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty (Config (..), defConfig, encodePretty', keyOrder)
import           Data.Bifunctor (first)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LBS
import           Data.Data (Data)
import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text

-- Why have we gone this route? The serialization format of `TxBody era`
-- differs from the CDDL. We serialize to an intermediate type in order to simplify
-- the specification of Plutus scripts and to avoid users having to think about
-- and construct redeemer pointers. However it turns out we can still serialize to
-- the ledger's CDDL format and maintain the convenient script witness specification
-- that the cli commands build and build-raw expose.
--
-- The long term plan is to have all relevant outputs from the cli to adhere to
-- the ledger's CDDL spec. Modifying the existing TextEnvelope machinery to encompass
-- this would result in a lot of unnecessary changes where the serialization
-- already defaults to the CDDL spec. In order to reduce the number of changes, and to
-- ease removal of the non-CDDL spec serialization, we have opted to create a separate
-- data type to encompass this in the interim.

data TextEnvelopeCddl = TextEnvelopeCddl
  { teCddlType :: !Text
  , teCddlDescription :: !Text
  , teCddlRawCBOR :: !ByteString
  } deriving (Eq, Show)

instance ToJSON TextEnvelopeCddl where
  toJSON TextEnvelopeCddl {teCddlType, teCddlDescription, teCddlRawCBOR} =
    object [ "type"        .= teCddlType
           , "description" .= teCddlDescription
           , "cborHex"     .= Text.decodeUtf8 (Base16.encode teCddlRawCBOR)
           ]

instance FromJSON TextEnvelopeCddl where
  parseJSON = withObject "TextEnvelopeCddl" $ \v ->
                TextEnvelopeCddl <$> (v .: "type")
                                 <*> (v .: "description")
                                 <*> (parseJSONBase16 =<< v .: "cborHex")
    where
      parseJSONBase16 v =
        either fail return . Base16.decode . Text.encodeUtf8 =<< parseJSON v


data TextEnvelopeCddlError
  = TextEnvelopeCddlErrCBORDecodingError DecoderError
  | TextEnvelopeCddlAesonDecodeError FilePath String
  | TextEnvelopeCddlUnknownKeyWitness
  | TextEnvelopeCddlTypeError
      [Text] -- ^ Expected types
      Text   -- ^ Actual types
  | TextEnvelopeCddlErrUnknownType Text
  | TextEnvelopeCddlErrByronKeyWitnessUnsupported
  deriving (Show, Eq, Data)

instance Error TextEnvelopeCddlError where
  prettyError = \case
    TextEnvelopeCddlErrCBORDecodingError decoderError ->
      "TextEnvelopeCDDL CBOR decoding error: " <> pshow decoderError
    TextEnvelopeCddlAesonDecodeError fp aesonErr ->
      mconcat
        [ "Could not JSON decode TextEnvelopeCddl file at: " <> pretty fp
        , " Error: " <> pretty aesonErr
        ]
    TextEnvelopeCddlUnknownKeyWitness ->
      "Unknown key witness specified"
    TextEnvelopeCddlTypeError expTypes actType ->
      mconcat
        [ "TextEnvelopeCddl type error: "
        , " Expected one of: "
        , mconcat $ List.intersperse ", " (map pretty expTypes)
        , " Actual: " <> pretty actType
        ]
    TextEnvelopeCddlErrUnknownType unknownType ->
      "Unknown TextEnvelopeCddl type: " <> pretty unknownType
    TextEnvelopeCddlErrByronKeyWitnessUnsupported ->
      "TextEnvelopeCddl error: Byron key witnesses are currently unsupported."

serialiseTxLedgerCddl :: ShelleyBasedEra era -> Tx era -> TextEnvelopeCddl
serialiseTxLedgerCddl era tx =
  shelleyBasedEraConstraints era $
    TextEnvelopeCddl
      { teCddlType = genType tx
      , teCddlDescription = "Ledger Cddl Format"
      , teCddlRawCBOR = serialiseToCBOR tx
      -- The SerialiseAsCBOR (Tx era) instance serializes to the Cddl format
      }
 where
  genType :: Tx era -> Text
  genType tx' = case getTxWitnesses tx' of
                  [] -> "Unwitnessed " <> genTxType
                  _ -> "Witnessed " <> genTxType
  genTxType :: Text
  genTxType =
    case era of
      ShelleyBasedEraShelley -> "Tx ShelleyEra"
      ShelleyBasedEraAllegra -> "Tx AllegraEra"
      ShelleyBasedEraMary -> "Tx MaryEra"
      ShelleyBasedEraAlonzo -> "Tx AlonzoEra"
      ShelleyBasedEraBabbage -> "Tx BabbageEra"
      ShelleyBasedEraConway -> "Tx ConwayEra"

deserialiseTxLedgerCddl :: ()
  => ShelleyBasedEra era
  -> TextEnvelopeCddl
  -> Either TextEnvelopeCddlError (Tx era)
deserialiseTxLedgerCddl era tec =
  first TextEnvelopeCddlErrCBORDecodingError . deserialiseTx era $ teCddlRawCBOR tec

writeByronTxFileTextEnvelopeCddl
  :: File content Out
  -> Byron.ATxAux ByteString
  -> IO (Either (FileError ()) ())
writeByronTxFileTextEnvelopeCddl path w =
  runExceptT $ do
    handleIOExceptT (FileIOError (unFile path)) $ LBS.writeFile (unFile path) txJson
  where
    txJson = encodePretty' textEnvelopeCddlJSONConfig (serializeByronTx w) <> "\n"

serializeByronTx :: Byron.ATxAux ByteString -> TextEnvelopeCddl
serializeByronTx tx =
  TextEnvelopeCddl
    { teCddlType = "Tx ByronEra"
    , teCddlDescription = "Ledger Cddl Format"
    , teCddlRawCBOR = CBOR.recoverBytes tx
    }

deserialiseByronTxCddl :: TextEnvelopeCddl -> Either TextEnvelopeCddlError (Byron.ATxAux ByteString)
deserialiseByronTxCddl tec =
   first TextEnvelopeCddlErrCBORDecodingError $
     CBOR.decodeFullAnnotatedBytes
        CBOR.byronProtVer "Byron Tx"
        CBOR.decCBOR (LBS.fromStrict $ teCddlRawCBOR tec)

deserialiseTx :: ()
  => ShelleyBasedEra era
  -> ByteString
  -> Either DecoderError (Tx era)
deserialiseTx sbe =
  shelleyBasedEraConstraints sbe
    $ deserialiseFromCBOR (AsTx (proxyToAsType Proxy))

serialiseWitnessLedgerCddl :: forall era. ShelleyBasedEra era -> KeyWitness era -> TextEnvelopeCddl
serialiseWitnessLedgerCddl sbe kw =
  TextEnvelopeCddl
    { teCddlType = witEra sbe
    , teCddlDescription = genDesc kw
    , teCddlRawCBOR = cddlSerialiseWitness kw
    }
 where
  cddlSerialiseWitness :: KeyWitness era -> ByteString
  cddlSerialiseWitness (ShelleyBootstrapWitness era wit) = CBOR.serialize' (eraProtVerLow era) wit
  cddlSerialiseWitness (ShelleyKeyWitness era wit) = CBOR.serialize' (eraProtVerLow era) wit
  cddlSerialiseWitness ByronKeyWitness{} = case sbe of {}

  genDesc :: KeyWitness era -> Text
  genDesc ByronKeyWitness{} = case sbe of {}
  genDesc ShelleyBootstrapWitness{} = "Key BootstrapWitness ShelleyEra"
  genDesc ShelleyKeyWitness{} = "Key Witness ShelleyEra"

  witEra :: ShelleyBasedEra era -> Text
  witEra ShelleyBasedEraShelley = "TxWitness ShelleyEra"
  witEra ShelleyBasedEraAllegra = "TxWitness AllegraEra"
  witEra ShelleyBasedEraMary = "TxWitness MaryEra"
  witEra ShelleyBasedEraAlonzo = "TxWitness AlonzoEra"
  witEra ShelleyBasedEraBabbage = "TxWitness BabbageEra"
  witEra ShelleyBasedEraConway = "TxWitness ConwayEra"

deserialiseWitnessLedgerCddl
  :: ShelleyBasedEra era
  -> TextEnvelopeCddl
  -> Either TextEnvelopeCddlError (KeyWitness era)
deserialiseWitnessLedgerCddl sbe TextEnvelopeCddl{teCddlRawCBOR,teCddlDescription} =
  --TODO: Parse these into types because this will increase code readability and
  -- will make it easier to keep track of the different Cddl descriptions via
  -- a single sum data type.
  case teCddlDescription of
    "Key BootstrapWitness ShelleyEra" -> do
      w <- first TextEnvelopeCddlErrCBORDecodingError
             $ CBOR.decodeFullAnnotator
               (eraProtVerLow sbe) "Shelley Witness" CBOR.decCBOR (LBS.fromStrict teCddlRawCBOR)
      Right $ ShelleyBootstrapWitness sbe w
    "Key Witness ShelleyEra" -> do
      w <- first TextEnvelopeCddlErrCBORDecodingError
             $ CBOR.decodeFullAnnotator
               (eraProtVerLow sbe) "Shelley Witness" CBOR.decCBOR (LBS.fromStrict teCddlRawCBOR)
      Right $ ShelleyKeyWitness sbe w
    _ -> Left TextEnvelopeCddlUnknownKeyWitness

writeTxFileTextEnvelopeCddl :: ()
  => ShelleyBasedEra era
  -> File content Out
  -> Tx era
  -> IO (Either (FileError ()) ())
writeTxFileTextEnvelopeCddl era path tx =
  runExceptT $ do
    handleIOExceptT (FileIOError (unFile path)) $ LBS.writeFile (unFile path) txJson
  where
    txJson = encodePretty' textEnvelopeCddlJSONConfig (serialiseTxLedgerCddl era tx) <> "\n"

writeTxWitnessFileTextEnvelopeCddl
  :: ShelleyBasedEra era
  -> File () Out
  -> KeyWitness era
  -> IO (Either (FileError ()) ())
writeTxWitnessFileTextEnvelopeCddl sbe path w =
  runExceptT $ do
    handleIOExceptT (FileIOError (unFile path)) $ LBS.writeFile (unFile path) txJson
  where
    txJson = encodePretty' textEnvelopeCddlJSONConfig (serialiseWitnessLedgerCddl sbe w) <> "\n"

textEnvelopeCddlJSONConfig :: Config
textEnvelopeCddlJSONConfig =
  defConfig { confCompare = textEnvelopeCddlJSONKeyOrder }

textEnvelopeCddlJSONKeyOrder :: Text -> Text -> Ordering
textEnvelopeCddlJSONKeyOrder = keyOrder ["type", "description", "cborHex"]

-- | This GADT allows us to deserialise a tx or key witness without
-- having to provide the era.
data FromSomeTypeCDDL c b where
  FromCDDLTx
    :: Text -- ^ CDDL type that we want
    -> (InAnyShelleyBasedEra Tx -> b)
    -> FromSomeTypeCDDL TextEnvelopeCddl b

  FromCDDLWitness
    :: Text -- ^ CDDL type that we want
    -> (InAnyShelleyBasedEra KeyWitness -> b)
    -> FromSomeTypeCDDL TextEnvelopeCddl b

deserialiseFromTextEnvelopeCddlAnyOf
  :: [FromSomeTypeCDDL TextEnvelopeCddl b]
  -> TextEnvelopeCddl
  -> Either TextEnvelopeCddlError b
deserialiseFromTextEnvelopeCddlAnyOf types teCddl =
    case List.find matching types of
      Nothing ->
        Left (TextEnvelopeCddlTypeError expectedTypes actualType)

      Just (FromCDDLTx ttoken f) -> do
        AnyShelleyBasedEra era <- cddlTypeToEra ttoken
        f . InAnyShelleyBasedEra era <$> deserialiseTxLedgerCddl era teCddl

      Just (FromCDDLWitness ttoken f) -> do
         AnyShelleyBasedEra era <- cddlTypeToEra ttoken
         f . InAnyShelleyBasedEra era <$> deserialiseWitnessLedgerCddl era teCddl
  where
   actualType :: Text
   actualType = teCddlType teCddl

   expectedTypes :: [Text]
   expectedTypes = [ typ | FromCDDLTx typ _f <- types ]

   matching :: FromSomeTypeCDDL TextEnvelopeCddl b -> Bool
   matching (FromCDDLTx ttoken _f) = actualType == ttoken
   matching (FromCDDLWitness ttoken _f)  = actualType == ttoken

-- Parse the text into types because this will increase code readability and
-- will make it easier to keep track of the different Cddl descriptions via
-- a single sum data type.
cddlTypeToEra :: Text -> Either TextEnvelopeCddlError AnyShelleyBasedEra
cddlTypeToEra = \case
  "Witnessed Tx ShelleyEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraShelley
  "Witnessed Tx AllegraEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraAllegra
  "Witnessed Tx MaryEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraMary
  "Witnessed Tx AlonzoEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraAlonzo
  "Witnessed Tx BabbageEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraBabbage
  "Witnessed Tx ConwayEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraConway
  "Unwitnessed Tx ShelleyEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraShelley
  "Unwitnessed Tx AllegraEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraAllegra
  "Unwitnessed Tx MaryEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraMary
  "Unwitnessed Tx AlonzoEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraAlonzo
  "Unwitnessed Tx BabbageEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraBabbage
  "Unwitnessed Tx ConwayEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraConway
  "TxWitness ShelleyEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraShelley
  "TxWitness AllegraEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraAllegra
  "TxWitness MaryEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraMary
  "TxWitness AlonzoEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraAlonzo
  "TxWitness BabbageEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraBabbage
  "TxWitness ConwayEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraConway
  unknownCddlType -> Left $ TextEnvelopeCddlErrUnknownType unknownCddlType

readFileTextEnvelopeCddlAnyOf
  :: [FromSomeTypeCDDL TextEnvelopeCddl b]
  -> FilePath
  -> IO (Either (FileError TextEnvelopeCddlError) b)
readFileTextEnvelopeCddlAnyOf types path =
  runExceptT $ do
    te <- newExceptT $ readTextEnvelopeCddlFromFile path
    firstExceptT (FileError path) $ hoistEither $ do
      deserialiseFromTextEnvelopeCddlAnyOf types te

readTextEnvelopeCddlFromFile
  :: FilePath
  -> IO (Either (FileError TextEnvelopeCddlError) TextEnvelopeCddl)
readTextEnvelopeCddlFromFile path =
  runExceptT $ do
    bs <- fileIOExceptT path readFileBlocking
    firstExceptT (FileError path . TextEnvelopeCddlAesonDecodeError path)
      . hoistEither $ Aeson.eitherDecodeStrict' bs
