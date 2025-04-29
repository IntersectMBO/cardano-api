{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.Internal.Keys.Read
  ( readKeyFile
  , readKeyFileTextEnvelope
  , readKeyFileAnyOf
  )
where

import Cardano.Api.Internal.DeserialiseAnyOf
import Cardano.Api.Internal.Error
import Cardano.Api.Internal.HasTypeProxy
import Cardano.Api.Internal.IO
import Cardano.Api.Internal.SerialiseBech32
import Cardano.Api.Internal.SerialiseTextEnvelope
import Cardano.Api.Internal.Utils

import Control.Monad.Except (runExceptT)
import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty)

-- | Read a cryptographic key from a file.
--
-- The contents of the file can either be Bech32-encoded, hex-encoded, or in
-- the text envelope format.
readKeyFile
  :: NonEmpty (InputFormat a)
  -> FilePath
  -> IO (Either (FileError InputDecodeError) a)
readKeyFile acceptedFormats path = do
  eContent <- runExceptT $ fileIOExceptT path readFileBlocking
  case eContent of
    Left e -> return $ Left e
    Right content ->
      return . first (FileError path) $ deserialiseInput acceptedFormats content

-- | Read a cryptographic key from a file.
--
-- The contents of the file must be in the text envelope format.
readKeyFileTextEnvelope
  :: HasTextEnvelope a
  => File content In
  -> IO (Either (FileError InputDecodeError) a)
readKeyFileTextEnvelope fp =
  first (fmap InputTextEnvelopeError) <$> readFileTextEnvelope fp

-- | Read a cryptographic key from a file given that it is one of the provided
-- types.
--
-- The contents of the file can either be Bech32-encoded or in the text
-- envelope format.
readKeyFileAnyOf
  :: forall content b
   . [FromSomeType SerialiseAsBech32 b]
  -> [FromSomeType HasTextEnvelope b]
  -> File content In
  -> IO (Either (FileError InputDecodeError) b)
readKeyFileAnyOf bech32Types textEnvTypes path = do
  eContent <- runExceptT $ fileIOExceptT (unFile path) readFileBlocking
  case eContent of
    Left e -> return $ Left e
    Right content ->
      return . first (FileError (unFile path)) $ deserialiseInputAnyOf bech32Types textEnvTypes content
