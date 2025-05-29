{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Internal utils for the other Api modules
module Cardano.Api.Internal.Utils
  ( (?!)
  , (?!.)
  , (<<$>>)
  , (<<<$>>>)
  , failEither
  , failEitherWith
  , noInlineMaybeToStrictMaybe
  , readFileBlocking
  , textShow

    -- ** CLI option parsing
  , unsafeBoundedRational
  )
where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Shelley ()

import Control.Exception (bracket)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable
import GHC.IO.Handle.FD (openFileBlocking)
import GHC.Stack
import System.IO (IOMode (ReadMode), hClose)

(?!) :: Maybe a -> e -> Either e a
Nothing ?! e = Left e
Just x ?! _ = Right x

(?!.) :: Either e a -> (e -> e') -> Either e' a
Left e ?!. f = Left (f e)
Right x ?!. _ = Right x

infixl 4 <<$>>

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

infixl 4 <<<$>>>

(<<<$>>>) :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
(<<<$>>>) = fmap . fmap . fmap

{-# NOINLINE noInlineMaybeToStrictMaybe #-}
noInlineMaybeToStrictMaybe :: Maybe a -> StrictMaybe a
noInlineMaybeToStrictMaybe Nothing = SNothing
noInlineMaybeToStrictMaybe (Just x) = SJust x

failEither :: MonadFail m => Either String a -> m a
failEither = either fail pure

failEitherWith :: MonadFail m => (e -> String) -> Either e a -> m a
failEitherWith f = either (fail . f) pure

readFileBlocking :: FilePath -> IO BS.ByteString
readFileBlocking path =
  bracket
    (openFileBlocking path ReadMode)
    hClose
    ( \fp -> do
        -- An arbitrary block size.
        let blockSize = 4096
        let go acc = do
              next <- BS.hGet fp blockSize
              if BS.null next
                then pure acc
                else go (acc <> Builder.byteString next)
        contents <- go mempty
        pure $ LBS.toStrict $ Builder.toLazyByteString contents
    )

textShow :: Show a => a -> Text
textShow = Text.pack . show

-- | Convert Rational to a bounded rational. Throw an exception when the rational is out of bounds.
unsafeBoundedRational
  :: forall r
   . (HasCallStack, Typeable r, BoundedRational r)
  => Rational
  -> r
unsafeBoundedRational x = fromMaybe (error errMessage) $ boundRational x
 where
  errMessage = show (typeRep (Proxy @r)) <> " is out of bounds: " <> show x
