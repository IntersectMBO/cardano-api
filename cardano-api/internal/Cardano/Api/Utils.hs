{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

-- | Internal utils for the other Api modules
module Cardano.Api.Utils
  ( (?!)
  , (?!.)
  , formatParsecError
  , failEither
  , failEitherWith
  , noInlineMaybeToStrictMaybe
  , note
  , readFileBlocking
  , runParsecParser
  , textShow

    -- ** CLI option parsing
  , unsafeBoundedRational
  )
where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Shelley ()
import Control.Exception (bracket)
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable
import GHC.IO.Handle.FD (openFileBlocking)
import GHC.Stack
import System.IO (IOMode (ReadMode), hClose)
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.String as Parsec
import qualified Text.ParserCombinators.Parsec.Error as Parsec

(?!) :: Maybe a -> e -> Either e a
Nothing ?! e = Left e
Just x ?! _ = Right x

(?!.) :: Either e a -> (e -> e') -> Either e' a
Left e ?!. f = Left (f e)
Right x ?!. _ = Right x

{-# NOINLINE noInlineMaybeToStrictMaybe #-}
noInlineMaybeToStrictMaybe :: Maybe a -> StrictMaybe a
noInlineMaybeToStrictMaybe Nothing = SNothing
noInlineMaybeToStrictMaybe (Just x) = SJust x

formatParsecError :: Parsec.ParseError -> String
formatParsecError err =
  Parsec.showErrorMessages
    "or"
    "unknown parse error"
    "expecting"
    "unexpected"
    "end of input"
    $ Parsec.errorMessages err

runParsecParser :: Parsec.Parser a -> Text -> Aeson.Parser a
runParsecParser parser input =
  case Parsec.parse (parser <* Parsec.eof) "" (Text.unpack input) of
    Right txin -> pure txin
    Left parseError -> fail $ formatParsecError parseError

failEither :: MonadFail m => Either String a -> m a
failEither = either fail pure

failEitherWith :: MonadFail m => (e -> String) -> Either e a -> m a
failEitherWith f = either (fail . f) pure

note :: MonadFail m => String -> Maybe a -> m a
note msg = \case
  Nothing -> fail msg
  Just a -> pure a

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
