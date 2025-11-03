{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}

module Cardano.Wasm.ExceptionHandling where

import Control.Exception (Exception, displayException)
import Control.Monad.Catch (MonadThrow (..))
import GHC.Exception (prettyCallStack)
import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)

data ExpectedJustException = HasCallStack => ExpectedJustException String

instance Show ExpectedJustException where
  show :: ExpectedJustException -> String
  show (ExpectedJustException msg) = "Expected Just, got Nothing: " ++ msg ++ "\n" ++ prettyCallStack callStack

instance Exception ExpectedJustException

data ExpectedRightException = HasCallStack => ExpectedRightException String

instance Show ExpectedRightException where
  show :: ExpectedRightException -> String
  show (ExpectedRightException msg) = "Expected Right, got Left: " ++ msg ++ "\n" ++ prettyCallStack callStack

instance Exception ExpectedRightException

data CustomException = HasCallStack => CustomException String

instance Show CustomException where
  show :: CustomException -> String
  show (CustomException msg) = "Custom exception: " ++ msg ++ "\n" ++ prettyCallStack callStack

instance Exception CustomException

throwError :: (HasCallStack, MonadThrow m) => String -> m a
throwError e = withFrozenCallStack $ throwM $ CustomException e

justOrError :: (HasCallStack, MonadThrow m) => String -> Maybe a -> m a
justOrError e Nothing = withFrozenCallStack $ throwM $ ExpectedJustException e
justOrError _ (Just a) = return a

rightOrError :: (HasCallStack, MonadThrow m, Show e) => Either e a -> m a
rightOrError (Left e) = withFrozenCallStack $ throwM $ ExpectedRightException $ show e
rightOrError (Right a) = return a

-- | Convert an 'Either' value to a 'MonadFail' monad. This can be useful for converting
-- MonadThrow monads into Aeson Parser monads, but it loses the stack trace information.
toMonadFail :: (Exception e, MonadFail m) => Either e a -> m a
toMonadFail (Left e) = fail $ displayException e
toMonadFail (Right a) = return a
