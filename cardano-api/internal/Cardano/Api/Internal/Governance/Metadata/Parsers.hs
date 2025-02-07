module Cardano.Api.Internal.Governance.Metadata.Parsers (textWithMaxLength) where

import           Data.Aeson.Types (Parser, Value, parseJSON)
import           Data.Text (Text)
import qualified Data.Text as T

-- | Parser for 'Text' that validates that the number of characters is
-- under a given maximum. The 'String' parameter is meant to be the name
-- of the field in order to be able to give context in case of error.
textWithMaxLength :: String -> Int -> Value -> Parser Text
textWithMaxLength fieldName maxLen value = do
  txt <- parseJSON value
  if T.length txt <= maxLen
    then pure txt
    else
      fail $
        "key \""
          ++ fieldName
          ++ "\" exceeds maximum length of "
          ++ show maxLen
          ++ " characters. Got length: "
          ++ show (T.length txt)
