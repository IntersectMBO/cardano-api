module Cardano.Api.Internal.Parser.String
  ( module Cardano.Api.Internal.Parser.String
  , module Control.Applicative
  , module Data.Functor
  , module Text.Parsec
  , module Text.Parsec.Char
  , module Text.Parsec.Expr
  , module Text.Parsec.String
  , module Text.ParserCombinators.Parsec.Combinator
  , module Char
  )
where

import Cardano.Api.Internal.Utils

import Control.Applicative
import Data.Char as Char (digitToInt)
import Data.Functor
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import Text.Parsec (ParseError, eof, notFollowedBy, parse, try, (<?>))
import Text.Parsec.Char (alphaNum, char, digit, hexDigit, space, spaces, string)
import Text.Parsec.Error (errorMessages, showErrorMessages)
import Text.Parsec.Expr (Assoc (..), Operator (..), buildExpressionParser)
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Combinator (many1)

-- | Word64 parser.
parseWord64 :: Parser Integer
parseWord64 = do
  i <- parseDecimal
  if i > fromIntegral (maxBound :: Word64)
    then
      fail $
        "expecting word64, but the number exceeds the max bound: " <> show i
    else return i

parseDecimal :: Parser Integer
parseDecimal = do
  digits <- many1 digit
  return $! foldl' (\x d -> 10 * x + toInteger (digitToInt d)) 0 digits

formatParsecError :: ParseError -> String
formatParsecError err =
  showErrorMessages
    "or"
    "unknown parse error"
    "expecting"
    "unexpected"
    "end of input"
    $ errorMessages err

runParsecParser :: Parser a -> Text -> Either String a
runParsecParser parser input =
  case parse (parser <* eof) "" (T.unpack input) of
    Right txin -> pure txin
    Left parseError -> Left $ formatParsecError parseError

runParsecParserFail :: MonadFail m => Parser a -> Text -> m a
runParsecParserFail p = failEither . runParsecParser p
