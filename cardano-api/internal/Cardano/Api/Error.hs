{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Class of errors used in the Api.
--
module Cardano.Api.Error
  ( Error(..)
  , throwErrorAsException
  , ErrorAsException(..)
  , FileError(..)
  , fileIOExceptT
  , displayError
  ) where

import           Cardano.Api.Pretty

import           Control.Exception (Exception (..), IOException, throwIO)
import           Control.Monad.Except (throwError)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (handleIOExceptT)
import           Prettyprinter
import           System.Directory (doesFileExist)
import           System.IO (Handle)

class Error e where
  prettyError :: e -> Doc ann

instance Error () where
  prettyError () = ""

-- | The preferred approach is to use 'Except' or 'ExceptT', but you can if
-- necessary use IO exceptions.
--
throwErrorAsException :: Error e => e -> IO a
throwErrorAsException e = throwIO (ErrorAsException e)

data ErrorAsException where
  ErrorAsException :: Error e => e -> ErrorAsException

instance Error ErrorAsException where
  prettyError (ErrorAsException e) =
    prettyError e

instance Show ErrorAsException where
  show (ErrorAsException e) =
    prettyToString $ prettyError e

instance Exception ErrorAsException where
  displayException (ErrorAsException e) =
    prettyToString $ prettyError e

displayError :: Error a => a -> String
displayError = prettyToString . prettyError

data FileError e = FileError FilePath e
                 | FileErrorTempFile
                     FilePath
                     -- ^ Target path
                     FilePath
                     -- ^ Temporary path
                     Handle
                 | FileDoesNotExistError FilePath
                 | FileIOError FilePath IOException
  deriving (Show, Eq, Functor)

instance Error e => Error (FileError e) where
  prettyError = \case
    FileErrorTempFile targetPath tempPath h ->
      vsep
        [ "Error creating temporary file at: " <> pretty tempPath
        , "Target path: " <> pretty targetPath
        , "Handle: " <> pshow h
        ]
    FileDoesNotExistError path ->
      "Error file not found at: " <> pretty path
    FileIOError path ioe ->
      pretty path <> ": " <> pretty (displayException ioe)
    FileError path e ->
      pretty path <> ": " <> prettyError e

instance Error IOException where
  prettyError = pretty . show

fileIOExceptT :: MonadIO m
              => FilePath
              -> (FilePath -> IO s)
              -> ExceptT (FileError e) m s
fileIOExceptT fp readFile' = do
  fileExists <- handleIOExceptT (FileIOError fp) $ doesFileExist fp
  if fileExists then handleIOExceptT (FileIOError fp) $ readFile' fp
                else throwError (FileDoesNotExistError fp)

