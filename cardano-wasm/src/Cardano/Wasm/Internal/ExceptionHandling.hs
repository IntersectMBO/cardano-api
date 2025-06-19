module Cardano.Wasm.Internal.ExceptionHandling where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow (..))
import GHC.Stack (HasCallStack, withFrozenCallStack)

newtype ExpectedJustException = ExpectedJustException String
  deriving Show

instance Exception ExpectedJustException

newtype ExpectedRightException = ExpectedRightException String
  deriving Show

instance Exception ExpectedRightException

justOrError :: (HasCallStack, MonadThrow m) => String -> Maybe a -> m a
justOrError e Nothing = withFrozenCallStack $ throwM $ ExpectedJustException e
justOrError _ (Just a) = return a

rightOrError :: (HasCallStack, MonadThrow m, Show e) => Either e a -> m a
rightOrError (Left e) = withFrozenCallStack $ throwM $ ExpectedRightException $ show e
rightOrError (Right a) = return a
