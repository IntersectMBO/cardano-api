module Cardano.Api.Pretty.Internal.ShowOf
  ( ShowOf (..)
  )
where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types
import Data.Text qualified as Text
import Prettyprinter

newtype ShowOf a = ShowOf a

instance Show a => Show (ShowOf a) where
  show (ShowOf a) = show a

instance Show a => Pretty (ShowOf a) where
  pretty = viaShow

instance Show a => ToJSON (ShowOf a) where
  toJSON (ShowOf a) = String (Text.pack (show a))

instance Show a => ToJSONKey (ShowOf a) where
  toJSONKey = toJSONKeyKey (Key.fromString . show)
