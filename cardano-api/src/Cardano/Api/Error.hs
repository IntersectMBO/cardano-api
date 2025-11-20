{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Class of errors used in the Api.
module Cardano.Api.Error
  ( Error (..)
  , throwErrorM
  , liftEitherError
  , failEitherError
  , ErrorAsException (..)
  , FileError (..)
  , fileIOExceptT
  , displayError
  , renderBuildable
  )
where

import Cardano.Api.Monad.Error
import Cardano.Api.Pretty

import Control.Exception.Safe
import GHC.Stack
import System.Directory (doesFileExist)
import System.IO (Handle)

class Error e where
  prettyError :: e -> Doc ann

instance Error () where
  prettyError () = ""

-- | Throw an 'Error e' as  'ErrorAsException'. 'throwErrorM' will attach a call stack to the exception.
throwErrorM
  :: HasCallStack
  => MonadThrow m
  => Typeable e
  => Error e
  => e
  -> m a
throwErrorM e = withFrozenCallStack $ throwM $ ErrorAsException e

-- | Pretty print 'Error e' and 'throwM' it wrapped in 'ErrorAsException' when 'Left'.
liftEitherError
  :: HasCallStack
  => MonadThrow m
  => Typeable e
  => Error e
  => Either e a
  -> m a
liftEitherError = withFrozenCallStack $ either throwErrorM pure

-- | Pretty print 'Error e' and 'fail' if 'Left'.
failEitherError
  :: MonadFail m
  => Error e
  => Either e a
  -> m a
failEitherError = failEitherWith displayError

-- | An exception wrapping any 'Error e', attaching a call stack from the construction place to it.
data ErrorAsException where
  ErrorAsException :: (HasCallStack, Typeable e, Error e) => e -> ErrorAsException

instance Exception ErrorAsException

-- | Pretty print the error inside the exception
instance Error ErrorAsException where
  prettyError (ErrorAsException e) =
    prettyError e

-- | Pretty print the error inside the exception followed by the call stack pointing to the place where 'Error e' was
-- wrapped in 'ErrorAsException'
instance Show ErrorAsException where
  show (ErrorAsException e) =
    docToString (prettyError e) <> "\n" <> prettyCallStack callStack

displayError :: Error a => a -> String
displayError = docToString . prettyError

data FileError e
  = FileError FilePath e
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

fileIOExceptT
  :: MonadIO m
  => FilePath
  -> (FilePath -> IO s)
  -> ExceptT (FileError e) m s
fileIOExceptT fp readFile' = do
  fileExists <- handleIOExceptT (FileIOError fp) $ doesFileExist fp
  if fileExists
    then handleIOExceptT (FileIOError fp) $ readFile' fp
    else throwError (FileDoesNotExistError fp)
