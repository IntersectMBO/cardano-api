{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}

module Cardano.Wasm.Internal.ExceptionHandling where

import Control.Exception (Exception, displayException)
import Control.Monad.Catch (MonadThrow (..))
import GHC.Exception (CallStack, prettyCallStack)
import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)

data ExpectedJustException = HasCallStack => ExpectedJustException CallStack String

instance Show ExpectedJustException where
  show :: ExpectedJustException -> String
  show (ExpectedJustException cs msg) = "Expected Just, got Nothing: " ++ msg ++ "\n" ++ prettyCallStack cs

instance Exception ExpectedJustException

data ExpectedRightException = HasCallStack => ExpectedRightException CallStack String

instance Show ExpectedRightException where
  show :: ExpectedRightException -> String
  show (ExpectedRightException cs msg) = "Expected Right, got Left: " ++ msg ++ "\n" ++ prettyCallStack cs

instance Exception ExpectedRightException

data CustomException = HasCallStack => CustomException CallStack String

instance Show CustomException where
  show :: CustomException -> String
  show (CustomException cs msg) = "Custom exception: " ++ msg ++ "\n" ++ prettyCallStack cs

instance Exception CustomException

throwError :: (HasCallStack, MonadThrow m) => String -> m a
throwError e = withFrozenCallStack $ throwM $ CustomException callStack e

justOrError :: (HasCallStack, MonadThrow m) => String -> Maybe a -> m a
justOrError e Nothing = withFrozenCallStack $ throwM $ ExpectedJustException callStack e
justOrError _ (Just a) = return a

rightOrError :: (HasCallStack, MonadThrow m, Show e) => Either e a -> m a
rightOrError (Left e) = withFrozenCallStack $ throwM $ ExpectedRightException callStack $ show e
rightOrError (Right a) = return a

-- | Convert an 'Either' value to a 'MonadFail' monad. This can be useful for converting
-- MonadThrow monads into Aeson Parser monads, but it loses the stack trace information.
toMonadFail :: (Exception e, MonadFail m) => Either e a -> m a
toMonadFail (Left e) = fail $ displayException e
toMonadFail (Right a) = return a
