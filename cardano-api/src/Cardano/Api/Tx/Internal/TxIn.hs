{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Transaction inputs
module Cardano.Api.Tx.Internal.TxIn
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

import Cardano.Api.HasTypeProxy
import Cardano.Api.Internal.Orphans.Misc ()
import Cardano.Api.Parser.Text qualified as P
import Cardano.Api.Pretty
import Cardano.Api.Serialise.Json
import Cardano.Api.Serialise.Raw
import Cardano.Api.Serialise.SerialiseUsing

import Cardano.Chain.UTxO qualified as Byron
import Cardano.Crypto.Hash.Class qualified as Crypto
import Cardano.Crypto.Hashing qualified as Byron
import Cardano.Ledger.BaseTypes (TxIx (..))
import Cardano.Ledger.Hashes qualified as Hashes
import Cardano.Ledger.Shelley.TxBody qualified as Shelley
import Cardano.Ledger.TxIn qualified as Ledger

import Data.Aeson (withText)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (ToJSONKey (..), toJSONKeyText)

-- ----------------------------------------------------------------------------
-- Transaction Ids
--

newtype TxId = TxId (Crypto.Hash Hashes.HASH Shelley.EraIndependentTxBody)
  -- We use the Shelley representation and convert to/from the Byron one
  deriving stock (Eq, Ord)
  deriving (Show, Pretty) via UsingRawBytesHex TxId
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

instance Pretty TxIn where
  pretty (TxIn txId txIx) = pretty txId <> "#" <> pretty txIx

parseTxIn :: P.Parser TxIn
parseTxIn = TxIn <$> parseTxId <*> (P.char '#' *> parseTxIx)

renderTxIn :: TxIn -> Text
renderTxIn = docToText . pretty

parseTxIx :: P.Parser TxIx
parseTxIx = TxIx . fromIntegral <$> P.parseDecimal

fromByronTxIn :: Byron.TxIn -> TxIn
fromByronTxIn (Byron.TxInUtxo txId index) =
  let shortBs = Byron.abstractHashToShort txId
      mApiHash = Crypto.hashFromBytesShort shortBs
   in case mApiHash of
        Just apiHash -> TxIn (TxId apiHash) (TxIx index)
        Nothing -> error $ "Error converting Byron era TxId: " <> show txId

toByronTxIn :: TxIn -> Byron.TxIn
toByronTxIn (TxIn txid (TxIx txix)) =
  Byron.TxInUtxo (toByronTxId txid) txix

toShelleyTxIn :: TxIn -> Ledger.TxIn
toShelleyTxIn (TxIn txid txix) =
  Ledger.TxIn (toShelleyTxId txid) txix

fromShelleyTxIn :: Ledger.TxIn -> TxIn
fromShelleyTxIn (Ledger.TxIn txid txix) =
  TxIn (fromShelleyTxId txid) txix
