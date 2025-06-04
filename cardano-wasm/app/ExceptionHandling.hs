module ExceptionHandling where

import Cardano.Api (liftEither)

import GHC.Stack (HasCallStack, withFrozenCallStack)

justOrError :: HasCallStack => String -> Maybe a -> a
justOrError e Nothing = withFrozenCallStack $ error e
justOrError _ (Just a) = a

rightOrError :: (HasCallStack, Show e) => Either e a -> a
rightOrError (Left e) = withFrozenCallStack $ error $ show e
rightOrError (Right a) = a
