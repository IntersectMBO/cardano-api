{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Transaction inputs
module Cardano.Api.Internal.TxIn
  ( -- * Transaction inputs
    TxIn (..)
  , TxIx (..)

    -- * Transaction Ids
  , TxId (..)
  , parseTxId

    -- * Data family instances
  , AsType (AsTxId)

    -- * Internal conversion functions
  , toByronTxId
  , toShelleyTxId
  , fromShelleyTxId
  , toByronTxIn
  , fromByronTxIn
  , toShelleyTxIn
  , fromShelleyTxIn
  , renderTxIn
  )
where

import Cardano.Api.Internal.Error
import Cardano.Api.Internal.HasTypeProxy
import Cardano.Api.Internal.Pretty
import Cardano.Api.Internal.SerialiseJSON
import Cardano.Api.Internal.SerialiseRaw
import Cardano.Api.Internal.SerialiseUsing
import Cardano.Api.Internal.Utils

import Cardano.Chain.UTxO qualified as Byron
import Cardano.Crypto.Hash.Class qualified as Crypto
import Cardano.Crypto.Hashing qualified as Byron
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Hashes qualified as Hashes
import Cardano.Ledger.Shelley.TxBody qualified as Shelley
import Cardano.Ledger.TxIn qualified as Ledger

import Control.Applicative (some)
import Data.Aeson (withText)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (ToJSONKey (..), toJSONKeyText)
import Data.ByteString.Char8 qualified as BSC
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Parsec ((<?>))
import Text.Parsec qualified as Parsec
import Text.Parsec.Language qualified as Parsec
import Text.Parsec.String qualified as Parsec
import Text.Parsec.Token qualified as Parsec

-- ----------------------------------------------------------------------------
-- Transaction Ids
--

newtype TxId = TxId (Crypto.Hash Hashes.HASH Shelley.EraIndependentTxBody)
  -- We use the Shelley representation and convert to/from the Byron one
  deriving stock (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex TxId
  deriving (ToJSON, FromJSON) via UsingRawBytesHex TxId
  deriving (ToJSONKey, FromJSONKey) via UsingRawBytesHex TxId

instance HasTypeProxy TxId where
  data AsType TxId = AsTxId
  proxyToAsType _ = AsTxId

instance SerialiseAsRawBytes TxId where
  serialiseToRawBytes (TxId h) = Crypto.hashToBytes h
  deserialiseFromRawBytes AsTxId bs = case Crypto.hashFromBytes bs of
    Just a -> Right (TxId a)
    Nothing -> Left $ SerialiseAsRawBytesError "Unable to deserialise TxId"

toByronTxId :: TxId -> Byron.TxId
toByronTxId (TxId h) =
  Byron.unsafeHashFromBytes (Crypto.hashToBytes h)

toShelleyTxId :: TxId -> Ledger.TxId
toShelleyTxId (TxId h) =
  Ledger.TxId (Hashes.unsafeMakeSafeHash (Crypto.castHash h))

fromShelleyTxId :: Ledger.TxId -> TxId
fromShelleyTxId (Ledger.TxId h) =
  TxId (Crypto.castHash (Hashes.extractHash h))

-- ----------------------------------------------------------------------------
-- Transaction inputs
--

data TxIn = TxIn TxId TxIx
  deriving (Eq, Ord, Show)

instance ToJSON TxIn where
  toJSON txIn = Aeson.String $ renderTxIn txIn

instance ToJSONKey TxIn where
  toJSONKey = toJSONKeyText renderTxIn

instance FromJSON TxIn where
  parseJSON = withText "TxIn" $ runParsecParser parseTxIn

instance FromJSONKey TxIn where
  fromJSONKey = Aeson.FromJSONKeyTextParser $ runParsecParser parseTxIn

deriving via (ShowOf TxIn) instance Pretty TxIn

parseTxId :: Parsec.Parser TxId
parseTxId = do
  str <- some Parsec.hexDigit <?> "transaction id (hexadecimal)"
  failEitherWith
    (\e -> docToString $ "Incorrect transaction id format: " <> prettyError e)
    (deserialiseFromRawBytesHex $ BSC.pack str)

parseTxIn :: Parsec.Parser TxIn
parseTxIn = TxIn <$> parseTxId <*> (Parsec.char '#' *> parseTxIx)

parseTxIx :: Parsec.Parser TxIx
parseTxIx = TxIx . fromIntegral <$> decimal

decimal :: Parsec.Parser Integer
Parsec.TokenParser{Parsec.decimal = decimal} = Parsec.haskell

renderTxIn :: TxIn -> Text
renderTxIn (TxIn txId (TxIx ix)) =
  serialiseToRawBytesHexText txId <> "#" <> Text.pack (show ix)

newtype TxIx = TxIx Word
  deriving stock (Eq, Ord, Show)
  deriving newtype Enum
  deriving newtype (ToJSON, FromJSON)

fromByronTxIn :: Byron.TxIn -> TxIn
fromByronTxIn (Byron.TxInUtxo txId index) =
  let shortBs = Byron.abstractHashToShort txId
      mApiHash = Crypto.hashFromBytesShort shortBs
   in case mApiHash of
        Just apiHash -> TxIn (TxId apiHash) (TxIx . fromIntegral $ toInteger index)
        Nothing -> error $ "Error converting Byron era TxId: " <> show txId

toByronTxIn :: TxIn -> Byron.TxIn
toByronTxIn (TxIn txid (TxIx txix)) =
  Byron.TxInUtxo (toByronTxId txid) (fromIntegral txix)

-- | This function may overflow on the transaction index. Call sites must ensure
-- that all uses of this function are appropriately guarded.
toShelleyTxIn :: TxIn -> Ledger.TxIn
toShelleyTxIn (TxIn txid (TxIx txix)) =
  Ledger.TxIn (toShelleyTxId txid) (Ledger.TxIx $ fromIntegral txix)

fromShelleyTxIn :: Ledger.TxIn -> TxIn
fromShelleyTxIn (Ledger.TxIn txid (Ledger.TxIx txix)) =
  TxIn (fromShelleyTxId txid) (TxIx (fromIntegral txix))
