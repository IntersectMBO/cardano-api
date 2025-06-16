module Cardano.Api.Parser.Text
  ( module Cardano.Api.Parser.Text
  , module Control.Applicative
  , module Data.Functor
  , module Text.Parsec
  , module Text.Parsec.Char
  , module Text.Parsec.Expr
  , module Text.Parsec.Text
  , module Text.ParserCombinators.Parsec.Combinator
  , module Data.Char
  )
where

import Cardano.Api.Monad.Error

import Control.Applicative
import Data.Bifunctor (first)
import Data.Char (digitToInt)
import Data.Foldable qualified as F
import Data.Functor
import Data.Text (Text)
import Data.Word (Word64)
import Text.Parsec hiding (many, optional, runParser, (<|>))
import Text.Parsec.Char
import Text.Parsec.Error (errorMessages, showErrorMessages)
import Text.Parsec.Expr (Assoc (..), Operator (..), buildExpressionParser)
import Text.Parsec.Text (Parser)
import Text.ParserCombinators.Parsec.Combinator (many1)

-- | Run parser
runParser
  :: Parser a
  -> Text
  -- ^ input text
  -> Either String a
runParser parser input =
  first formatParsecError $
    parse (parser <* eof) "" input
 where
  formatParsecError :: ParseError -> String
  formatParsecError err =
    showErrorMessages
      "or"
      "unknown parse error"
      "expecting"
      "unexpected"
      "end of input"
      $ errorMessages err

-- | Run parser in 'MonadFail'
runParserFail :: MonadFail m => Parser a -> Text -> m a
runParserFail p = failEither . runParser p

-- | Word64 parser.
parseWord64 :: Parser Integer
parseWord64 = do
  i <- parseDecimal
  if i > fromIntegral (maxBound :: Word64)
    then
      fail $
        "expecting word64, but the number exceeds the max bound: " <> show i
    else return i

-- | Non-negative decimal numbers parser
parseDecimal :: Parser Integer
parseDecimal = do
  digits <- many1 digit
  return $! F.foldl' (\x d -> 10 * x + toInteger (digitToInt d)) 0 digits
