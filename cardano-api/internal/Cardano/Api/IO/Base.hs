{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Api.IO.Base
  ( FileDirection(..)
  , File(..)
  , Socket
  , SocketPath
  , VRFPrivateKeyFilePermissionError(..)
  ) where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.String (IsString)

data FileDirection
  = In
  -- ^ Indicate the file is to be used for reading.
  | Out
  -- ^ Indicate the file is to be used for writing.
  | InOut
  -- ^ Indicate the file is to be used for both reading and writing.

-- | A file path with additional type information to indicate what the file is meant to
-- contain and whether it is to be used for reading or writing.
newtype File content (direction :: FileDirection) = File
  { unFile :: FilePath
  } deriving newtype (Eq, Ord, Read, Show, IsString, FromJSON, ToJSON)

data Socket

type SocketPath = File Socket 'InOut

data VRFPrivateKeyFilePermissionError
  = OtherPermissionsExist FilePath
  | GroupPermissionsExist FilePath
  | GenericPermissionsExist FilePath
  deriving Show
