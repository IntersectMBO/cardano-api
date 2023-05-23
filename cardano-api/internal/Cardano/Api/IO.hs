{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Api.IO
  ( readByteStringFile
  , readLazyByteStringFile
  , readTextFile

  , writeByteStringFileWithOwnerPermissions
  , writeByteStringFile
  , writeByteStringOutput

  , writeLazyByteStringFileWithOwnerPermissions
  , writeLazyByteStringFile
  , writeLazyByteStringOutput

  , writeTextFileWithOwnerPermissions
  , writeTextFile
  , writeTextOutput

  , File(..)
  , FileDirection(..)
  , SocketPath

  , mapFile
  , onlyIn
  , onlyOut

  , intoFile

  , writeSecrets
  ) where

import           Cardano.Api.Error (FileError (..))
import           Cardano.Api.IO.Compat

import           Control.Monad.Except (runExceptT)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Except.Extra (handleIOExceptT)
import           Data.Aeson (FromJSON, ToJSON)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy as LBSC
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text.IO as Text

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

readByteStringFile :: ()
  => MonadIO m
  => File content In
  -> m (Either (FileError e) ByteString)
readByteStringFile fp = runExceptT $
  handleIOExceptT (FileIOError (unFile fp)) $ BS.readFile (unFile fp)

readLazyByteStringFile :: ()
  => MonadIO m
  => File content In
  -> m (Either (FileError e) LBS.ByteString)
readLazyByteStringFile fp = runExceptT $
  handleIOExceptT (FileIOError (unFile fp)) $ LBS.readFile (unFile fp)

readTextFile :: ()
  => MonadIO m
  => File content In
  -> m (Either (FileError e) Text)
readTextFile fp = runExceptT $
  handleIOExceptT (FileIOError (unFile fp)) $ Text.readFile (unFile fp)

writeByteStringFile :: ()
  => MonadIO m
  => File content Out
  -> ByteString
  -> m (Either (FileError e) ())
writeByteStringFile fp bs = runExceptT $
  handleIOExceptT (FileIOError (unFile fp)) $ BS.writeFile (unFile fp) bs

writeByteStringFileWithOwnerPermissions
  :: FilePath
  -> BS.ByteString
  -> IO (Either (FileError e) ())
writeByteStringFileWithOwnerPermissions fp bs =
  handleFileForWritingWithOwnerPermission fp $ \h ->
    BS.hPut h bs

writeByteStringOutput :: ()
  => MonadIO m
  => Maybe (File content Out)
  -> ByteString
  -> m (Either (FileError e) ())
writeByteStringOutput mOutput bs = runExceptT $
  case mOutput of
    Just fp -> handleIOExceptT (FileIOError (unFile fp)) $ BS.writeFile (unFile fp) bs
    Nothing -> liftIO $ BSC.putStr bs

writeLazyByteStringFile :: ()
  => MonadIO m
  => File content Out
  -> LBS.ByteString
  -> m (Either (FileError e) ())
writeLazyByteStringFile fp bs = runExceptT $
  handleIOExceptT (FileIOError (unFile fp)) $ LBS.writeFile (unFile fp) bs

writeLazyByteStringFileWithOwnerPermissions
  :: File content Out
  -> LBS.ByteString
  -> IO (Either (FileError e) ())
writeLazyByteStringFileWithOwnerPermissions fp lbs =
  handleFileForWritingWithOwnerPermission (unFile fp) $ \h ->
    LBS.hPut h lbs

writeLazyByteStringOutput :: ()
  => MonadIO m
  => Maybe (File content Out)
  -> LBS.ByteString
  -> m (Either (FileError e) ())
writeLazyByteStringOutput mOutput bs = runExceptT $
  case mOutput of
    Just fp -> handleIOExceptT (FileIOError (unFile fp)) $ LBS.writeFile (unFile fp) bs
    Nothing -> liftIO $ LBSC.putStr bs

writeTextFile :: ()
  => MonadIO m
  => File content Out
  -> Text
  -> m (Either (FileError e) ())
writeTextFile fp t = runExceptT $
  handleIOExceptT (FileIOError (unFile fp)) $ Text.writeFile (unFile fp) t

writeTextFileWithOwnerPermissions
  :: File content Out
  -> Text
  -> IO (Either (FileError e) ())
writeTextFileWithOwnerPermissions fp t =
  handleFileForWritingWithOwnerPermission (unFile fp) $ \h ->
    Text.hPutStr h t

writeTextOutput :: ()
  => MonadIO m
  => Maybe (File content Out)
  -> Text
  -> m (Either (FileError e) ())
writeTextOutput mOutput t = runExceptT $
  case mOutput of
    Just fp -> handleIOExceptT (FileIOError (unFile fp)) $ Text.writeFile (unFile fp) t
    Nothing -> liftIO $ Text.putStr t

mapFile :: (FilePath -> FilePath) -> File content direction -> File content direction
mapFile f = File . f . unFile

onlyIn :: File content InOut -> File content In
onlyIn = File . unFile

onlyOut :: File content InOut -> File content Out
onlyOut = File . unFile

-- | Given a way to serialise a value and a way to write the stream to a file, serialise
-- a value into a stream, and write it to a file.
--
-- Whilst it is possible to call the serialisation and writing functions separately,
-- doing so means the compiler is unable to match the content type of the file with
-- the type of the content being serialised.
--
-- Using this function ensures that the content type of the file always matches with the
-- content value and prevents any type mismatches.
intoFile :: ()
  => File content 'Out
  -> content
  -> (File content 'Out -> stream -> result)
  -> (content -> stream)
  -> result
intoFile fp content write serialise = write fp (serialise content)

data Socket

type SocketPath = File Socket 'InOut
