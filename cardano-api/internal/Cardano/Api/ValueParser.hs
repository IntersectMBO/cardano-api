{-# LANGUAGE OverloadedLists #-}

module Cardano.Api.ValueParser
  ( parseValue
  , parseTxOutMultiAssetValue
  , parseMintingMultiAssetValue
  , parseUTxOValue
  , parseAssetName
  , parsePolicyId
  , ParserValueRole (..)
  )
where

import           Cardano.Api.Error (displayError)
import           Cardano.Api.SerialiseRaw
import           Cardano.Api.Utils (failEitherWith)
import           Cardano.Api.Value

import           Control.Applicative (many, some, (<|>))
import           Control.Monad (unless, when)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Char as Char
import           Data.Functor (void, ($>))
import           Data.List as List (foldl')
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Word (Word64)
import           Text.Parsec as Parsec (notFollowedBy, try, (<?>))
import           Text.Parsec.Char (alphaNum, char, digit, hexDigit, space, spaces, string)
import           Text.Parsec.Expr (Assoc (..), Operator (..), buildExpressionParser)
import           Text.Parsec.String (Parser)
import           Text.ParserCombinators.Parsec.Combinator (many1)

-- | The role for which a 'Value' is being parsed.
data ParserValueRole
  = -- | The value is used as a UTxO or transaction output.
    RoleUTxO
  | -- | The value is used as a minting policy.
    RoleMint
  deriving (Eq, Show, Enum, Bounded)

-- | Parse a 'Value' from its string representation. The @role@ argument for which purpose
-- the value is being parsed. This is used to enforce additional constraints on the value.
-- Why do we parse a general value and check for additional constraints you may ask?
-- Because we can't rule out the negation operator
-- for transaction outputs: some users have negative values in additions, with the addition's total
-- summing up to a positive value. So forbidding negations altogether is too restrictive.
parseValue :: ParserValueRole -> Parser Value
parseValue role = do
  valueExpr <- parseValueExpr
  let value = evalValueExpr valueExpr
  case role of
    RoleUTxO -> do
      unless (allPositive value) $
        fail $
          "Value must be positive in UTxO (or transaction output): " <> show value
      return value
    RoleMint -> do
      let (Coin lovelace) = selectLovelace value
      when (lovelace /= 0) $
        fail $
          "Lovelace must be zero in minting value: " <> show value
      return value

parseTxOutMultiAssetValue :: Parser Value
parseTxOutMultiAssetValue = parseValue RoleUTxO

parseMintingMultiAssetValue :: Parser Value
parseMintingMultiAssetValue = parseValue RoleMint

parseUTxOValue :: Parser Value
parseUTxOValue = parseValue RoleUTxO

-- | Evaluate a 'ValueExpr' and construct a 'Value'.
evalValueExpr :: ValueExpr -> Value
evalValueExpr vExpr =
  case vExpr of
    ValueExprAdd x y -> evalValueExpr x <> evalValueExpr y
    ValueExprNegate x -> negateValue (evalValueExpr x)
    ValueExprLovelace quant -> [(AdaAssetId, quant)]
    ValueExprMultiAsset polId aName quant -> [(AssetId polId aName, quant)]

------------------------------------------------------------------------------
-- Expression parser
------------------------------------------------------------------------------

-- | Intermediate representation of a parsed multi-asset value.
data ValueExpr
  = ValueExprAdd !ValueExpr !ValueExpr
  | ValueExprNegate !ValueExpr
  | ValueExprLovelace !Quantity
  | ValueExprMultiAsset !PolicyId !AssetName !Quantity
  deriving (Eq, Ord, Show)

parseValueExpr :: Parser ValueExpr
parseValueExpr =
  buildExpressionParser operatorTable valueExprTerm
    <?> "multi-asset value expression"
 where
  operatorTable =
    [ [Prefix negateOp]
    , [Infix plusOp AssocLeft]
    ]

-- | Parse either a 'ValueExprLovelace' or 'ValueExprMultiAsset'.
valueExprTerm :: Parser ValueExpr
valueExprTerm = do
  q <- try quantity <?> "quantity (word64)"
  aId <- try assetIdUnspecified <|> assetIdSpecified <?> "asset id"
  _ <- spaces
  pure $ case aId of
    AdaAssetId -> ValueExprLovelace q
    AssetId polId aName -> ValueExprMultiAsset polId aName q
 where
  -- Parse an asset ID which must be lead by one or more whitespace
  -- characters and may be trailed by whitespace characters.
  assetIdSpecified :: Parser AssetId
  assetIdSpecified = some space *> assetId

  -- Default for if an asset ID is not specified.
  assetIdUnspecified :: Parser AssetId
  assetIdUnspecified =
    spaces
      *> notFollowedBy alphaNum
      $> AdaAssetId

------------------------------------------------------------------------------
-- Primitive parsers
------------------------------------------------------------------------------

plusOp :: Parser (ValueExpr -> ValueExpr -> ValueExpr)
plusOp = (char '+' *> spaces) $> ValueExprAdd

negateOp :: Parser (ValueExpr -> ValueExpr)
negateOp = (char '-' *> spaces) $> ValueExprNegate

-- | Period (\".\") parser.
period :: Parser ()
period = void $ char '.'

-- | Word64 parser.
word64 :: Parser Integer
word64 = do
  i <- decimal
  if i > fromIntegral (maxBound :: Word64)
    then
      fail $
        "expecting word64, but the number exceeds the max bound: " <> show i
    else return i

decimal :: Parser Integer
decimal = do
  digits <- many1 digit
  return $! List.foldl' (\x d -> 10 * x + toInteger (Char.digitToInt d)) 0 digits

-- | Asset name parser.
parseAssetName :: Parser AssetName
parseAssetName = do
  hexText <- many hexDigit
  failEitherWith
    (\e -> "AssetName deserisalisation failed: " ++ displayError e)
    $ deserialiseFromRawBytesHex AsAssetName
    $ BSC.pack hexText

-- | Policy ID parser.
parsePolicyId :: Parser PolicyId
parsePolicyId = do
  hexText <- many1 hexDigit
  failEitherWith
    ( \e ->
        fail $
          "expecting a 56-hex-digit policy ID, but found "
            ++ show (length hexText)
            ++ " hex digits; "
            ++ displayError e
    )
    (textToPolicyId hexText)
 where
  textToPolicyId =
    fmap PolicyId
      . deserialiseFromRawBytesHex AsScriptHash
      . Text.encodeUtf8
      . Text.pack

-- | Asset ID parser.
assetId :: Parser AssetId
assetId =
  try adaAssetId
    <|> nonAdaAssetId
    <?> "asset ID"
 where
  -- Parse the ADA asset ID.
  adaAssetId :: Parser AssetId
  adaAssetId = string "lovelace" $> AdaAssetId

  -- Parse a multi-asset ID.
  nonAdaAssetId :: Parser AssetId
  nonAdaAssetId = do
    polId <- parsePolicyId
    fullAssetId polId <|> assetIdNoAssetName polId

  -- Parse a fully specified multi-asset ID with both a policy ID and asset
  -- name.
  fullAssetId :: PolicyId -> Parser AssetId
  fullAssetId polId = do
    _ <- period
    aName <- parseAssetName <?> "hexadecimal asset name"
    pure (AssetId polId aName)

  -- Parse a multi-asset ID that specifies a policy ID, but no asset name.
  assetIdNoAssetName :: PolicyId -> Parser AssetId
  assetIdNoAssetName polId = pure (AssetId polId "")

-- | Quantity (word64) parser. Only accepts positive quantities.
quantity :: Parser Quantity
quantity = fmap Quantity word64
