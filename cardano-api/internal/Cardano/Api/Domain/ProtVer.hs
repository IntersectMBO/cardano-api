{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Api.Domain.ProtVer
  ( ProtVer(..)
  , reLedgerProtVerL
  , unLedgerProtVerL
  ) where

import qualified Cardano.Ledger.BaseTypes as Ledger

import           Control.Monad.Trans.Fail.String (runFail)
import           Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import           Lens.Micro (Lens', lens)
import           Numeric.Natural (Natural)

newtype ProtVer = ProtVer
  { unProtVer :: Ledger.ProtVer
  }

instance FromJSON ProtVer where
  parseJSON v =
    flip (withObject "ProtocolParameters") v $ \o -> do
      major :: Natural <- o .: "major"
      minor :: Natural <- o .: "minor"
      let result = runFail $ (`Ledger.ProtVer` minor) <$> Ledger.mkVersion major
      case result of
        Right protVer -> pure $ ProtVer protVer
        Left msg -> fail $ "Invalid protocol version: " <> show v <> " (" <> msg <> ")"

instance ToJSON ProtVer where
  toJSON (ProtVer (Ledger.ProtVer a b)) =
    object
      [ "major" .= (Ledger.getVersion a :: Natural)
      , "minor" .= (b :: Natural)
      ]

reLedgerProtVerL :: Lens' ProtVer Ledger.ProtVer
reLedgerProtVerL = lens unProtVer (const ProtVer)

unLedgerProtVerL :: Lens' Ledger.ProtVer ProtVer
unLedgerProtVerL = lens ProtVer (const unProtVer)
