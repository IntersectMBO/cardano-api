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
  , parseTxIn
  , TxIx (..)
  , parseTxIx

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

import Cardano.Api.Internal.HasTypeProxy
import Cardano.Api.Internal.Pretty
import Cardano.Api.Internal.SerialiseJSON
import Cardano.Api.Internal.SerialiseRaw
import Cardano.Api.Internal.SerialiseUsing
import Cardano.Api.Parser.Text qualified as P

import Cardano.Chain.UTxO qualified as Byron
import Cardano.Crypto.Hash.Class qualified as Crypto
import Cardano.Crypto.Hashing qualified as Byron
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Hashes qualified as Hashes
import Cardano.Ledger.Shelley.TxBody qualified as Shelley
import Cardano.Ledger.TxIn qualified as Ledger

import Data.Aeson (withText)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (ToJSONKey (..), toJSONKeyText)
import Data.Text (Text)
import Data.Text qualified as Text

-- ----------------------------------------------------------------------------
-- Transaction Ids
--

newtype TxId = TxId (Crypto.Hash Hashes.HASH Shelley.EraIndependentTxBody)
  -- We use the Shelley representation and convert to/from the Byron one
  deriving stock (Eq, Ord)
  deriving Show via UsingRawBytesHex TxId
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

parseTxId :: P.Parser TxId
parseTxId = parseRawBytesHex

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
  parseJSON = withText "TxIn" $ P.runParserFail parseTxIn

instance FromJSONKey TxIn where
  fromJSONKey = Aeson.FromJSONKeyTextParser $ P.runParserFail parseTxIn

deriving via (ShowOf TxIn) instance Pretty TxIn

parseTxIn :: P.Parser TxIn
parseTxIn = TxIn <$> parseTxId <*> (P.char '#' *> parseTxIx)

renderTxIn :: TxIn -> Text
renderTxIn (TxIn txId (TxIx ix)) =
  serialiseToRawBytesHexText txId <> "#" <> Text.pack (show ix)

newtype TxIx = TxIx Word
  deriving stock (Eq, Ord, Show)
  deriving newtype Enum
  deriving newtype (ToJSON, FromJSON)

parseTxIx :: P.Parser TxIx
parseTxIx = TxIx . fromIntegral <$> P.parseDecimal

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
