{-# LANGUAGE OverloadedLists #-}

module Cardano.Api.Internal.ValueParser
  ( parseTxOutMultiAssetValue
  , parseMintingMultiAssetValue
  , parseUTxOValue
  , parseAssetName
  , parsePolicyId
  )
where

import Cardano.Api.Internal.Eon.MaryEraOnwards
import Cardano.Api.Internal.Value
import Cardano.Api.Parser.Text

import Cardano.Ledger.Mary.Value qualified as L

import Control.Monad (unless, when)

-- | Parse a 'Value' from its string representation. The resulting amounts must be positive for the parser
-- to succeed.
parseTxOutMultiAssetValue :: Parser Value
parseTxOutMultiAssetValue = parseValue RoleUTxO

-- | Parse a 'MintValue' from its string representation. The string representation cannot contain ADA.
parseMintingMultiAssetValue :: MaryEraOnwards era -> Parser L.MultiAsset
parseMintingMultiAssetValue w = maryEraOnwardsConstraints w $ do
  L.MaryValue 0 result <- toLedgerValue w <$> parseValue RoleMint
  pure result

-- | Parse a 'Value' from its string representation. The resulting amounts must be positive for the parser
-- to succeed.
parseUTxOValue :: Parser Value
parseUTxOValue = parseValue RoleUTxO

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
  buildExpressionParser operatorTable parseValueExprTerm
    <?> "multi-asset value expression"
 where
  operatorTable =
    [ [Prefix parseNegateOp]
    , [Infix parsePlusOp AssocLeft]
    ]

-- | Parse either a 'ValueExprLovelace' or 'ValueExprMultiAsset'.
parseValueExprTerm :: Parser ValueExpr
parseValueExprTerm = do
  q <- try parseQuantity <?> "quantity (word64)"
  aId <- try parseAssetIdUnspecified <|> parseAssetIdSpecified <?> "asset id"
  _ <- spaces
  pure $ case aId of
    AdaAssetId -> ValueExprLovelace q
    AssetId polId aName -> ValueExprMultiAsset polId aName q
 where
  -- Parse an asset ID which must be lead by one or more whitespace
  -- characters and may be trailed by whitespace characters.
  parseAssetIdSpecified :: Parser AssetId
  parseAssetIdSpecified = some space *> parseAssetId

  -- Default for if an asset ID is not specified.
  parseAssetIdUnspecified :: Parser AssetId
  parseAssetIdUnspecified =
    spaces
      *> notFollowedBy alphaNum
        $> AdaAssetId

------------------------------------------------------------------------------
-- Primitive parsers
------------------------------------------------------------------------------

parsePlusOp :: Parser (ValueExpr -> ValueExpr -> ValueExpr)
parsePlusOp = (char '+' *> spaces) $> ValueExprAdd

parseNegateOp :: Parser (ValueExpr -> ValueExpr)
parseNegateOp = (char '-' *> spaces) $> ValueExprNegate
