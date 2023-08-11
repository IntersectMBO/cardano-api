{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.Domain.PraosNonce
  ( PraosNonce (..)
  , makePraosNonce
  , toLedgerNonce
  , fromLedgerNonce

  , reLedgerNonceL
  , unLedgerNonceL
  , rePraosNonceHashL
  , unPraosNonceHashL
  ) where

import           Cardano.Api.Address
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Orphans ()
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseRaw
import           Cardano.Api.SerialiseUsing

import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Ledger.BaseTypes as Ledger
import           Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Keys as Ledger

import           Data.Aeson (FromJSON (..), ToJSON (..))
import           Data.ByteString (ByteString)
import           Data.Either.Combinators (maybeToRight)
import           Data.String (IsString)
import           GHC.Generics
import           Lens.Micro (Lens', lens)

newtype PraosNonce = PraosNonce
  { praosNonceHash :: Ledger.Hash StandardCrypto ByteString
  }
  deriving stock (Eq, Ord, Generic)
  deriving (Show, IsString)   via UsingRawBytesHex PraosNonce
  deriving (ToJSON, FromJSON) via UsingRawBytesHex PraosNonce
  deriving (ToCBOR, FromCBOR) via UsingRawBytes    PraosNonce

instance HasTypeProxy PraosNonce where
    data AsType PraosNonce = AsPraosNonce
    proxyToAsType _ = AsPraosNonce

instance SerialiseAsRawBytes PraosNonce where
    serialiseToRawBytes (PraosNonce h) =
      Crypto.hashToBytes h

    deserialiseFromRawBytes AsPraosNonce bs =
      maybeToRight (SerialiseAsRawBytesError "Unable to deserialise PraosNonce") $
        PraosNonce <$> Crypto.hashFromBytes bs

makePraosNonce :: ByteString -> PraosNonce
makePraosNonce = PraosNonce . Crypto.hashWith id

toLedgerNonce :: Maybe PraosNonce -> Ledger.Nonce
toLedgerNonce Nothing               = Ledger.NeutralNonce
toLedgerNonce (Just (PraosNonce h)) = Ledger.Nonce (Crypto.castHash h)

fromLedgerNonce :: Ledger.Nonce -> Maybe PraosNonce
fromLedgerNonce Ledger.NeutralNonce = Nothing
fromLedgerNonce (Ledger.Nonce h)    = Just (PraosNonce (Crypto.castHash h))

reLedgerNonceL :: Lens' (Maybe PraosNonce) Ledger.Nonce
reLedgerNonceL = lens toLedgerNonce (const fromLedgerNonce)

unLedgerNonceL :: Lens' Ledger.Nonce (Maybe PraosNonce)
unLedgerNonceL = lens fromLedgerNonce (const toLedgerNonce)

rePraosNonceHashL :: Lens' PraosNonce (Ledger.Hash StandardCrypto ByteString)
rePraosNonceHashL = lens praosNonceHash (const PraosNonce)

unPraosNonceHashL :: Lens' (Ledger.Hash StandardCrypto ByteString) PraosNonce
unPraosNonceHashL = lens PraosNonce (const praosNonceHash)
