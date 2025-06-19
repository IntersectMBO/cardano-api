module Cardano.Wasm.General.ExceptionHandling where

import Control.Exception (Exception, throwIO)
import GHC.Stack (HasCallStack, withFrozenCallStack)

newtype ExpectedRightException = ExpectedRightException String
  deriving Show

instance Exception ExpectedRightException

justOrError :: HasCallStack => String -> Maybe a -> a
justOrError e Nothing = withFrozenCallStack $ error e
justOrError _ (Just a) = a

rightOrError :: (HasCallStack, Show e) => Either e a -> a
rightOrError (Left e) = withFrozenCallStack $ error $ show e
rightOrError (Right a) = a

rightOrErrorM :: (HasCallStack, Show e) => Either e a -> IO a
rightOrErrorM (Left e) = withFrozenCallStack $ throwIO $ ExpectedRightException (show e)
rightOrErrorM (Right a) = return a
