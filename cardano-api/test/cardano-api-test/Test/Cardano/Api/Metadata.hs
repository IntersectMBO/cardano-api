{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Cardano.Api.Metadata
  ( tests
  , genTxMetadata
  , genTxMetadataValue
  )
where

import Cardano.Api

import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Word (Word64)
import GHC.Exts (IsList (..))
import GHC.Stack
import Text.RawString.QQ

import Test.Gen.Cardano.Api.Metadata

import Hedgehog (Gen, Property, (===))
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

-- ----------------------------------------------------------------------------
-- Golden / unit tests
--

prop_golden_1 :: Property
prop_golden_1 =
  matchMetadata
    TxMetadataJsonNoSchema
    [r|{"0": 1}|]
    (TxMetadata (fromList [(0, TxMetaNumber 1)]))

prop_golden_2 :: Property
prop_golden_2 =
  matchMetadata
    TxMetadataJsonNoSchema
    [r|{"0": "deadbeef"}|]
    (txMetadataSingleton 0 (TxMetaText "deadbeef"))

prop_golden_3 :: Property
prop_golden_3 =
  matchMetadata
    TxMetadataJsonNoSchema
    [r|{"0": "0xDEADBEEF"}|]
    (txMetadataSingleton 0 (TxMetaText "0xDEADBEEF"))

prop_golden_4 :: Property
prop_golden_4 =
  matchMetadata
    TxMetadataJsonNoSchema
    [r|{"0": "0xdeadbeef"}|]
    (txMetadataSingleton 0 (TxMetaBytes "\xde\xad\xbe\xef"))

prop_golden_5 :: Property
prop_golden_5 =
  matchMetadata
    TxMetadataJsonNoSchema
    [r|{"0": [] }|]
    (txMetadataSingleton 0 (TxMetaList []))

prop_golden_6 :: Property
prop_golden_6 =
  matchMetadata
    TxMetadataJsonNoSchema
    [r|{"0": [1, "a", "0x42"] }|]
    ( txMetadataSingleton
        0
        ( TxMetaList
            [ TxMetaNumber 1
            , TxMetaText "a"
            , TxMetaBytes "\x42"
            ]
        )
    )

prop_golden_7 :: Property
prop_golden_7 =
  matchMetadata
    TxMetadataJsonNoSchema
    [r|{"0": {} }|]
    (txMetadataSingleton 0 (TxMetaMap []))

prop_golden_8 :: Property
prop_golden_8 =
  matchMetadata
    TxMetadataJsonNoSchema
    [r|{"0": {
        "0x41": "0x42",
        "0x154041": "0x44",
        "0x104041": "0x43",
        "0x3041": "0x45",
        "aab": "ba",
        "abb": "ba",
        "11": 3,
        "1": 2,
        "a": "b",
        "aa": "bb",
        "ab": "ba",
        "aba": {
          "0x41": "0x42",
          "0x154041": "0x44",
          "0x104041": "0x43",
          "0x3041": "0x45",
          "aab": "ba",
          "abb": "ba",
          "11": 3,
          "1": 2,
          "a": "b",
          "aa": "bb",
          "ab": "ba"
         }
       }}|]
    ( txMetadataSingleton
        0
        ( TxMetaMap
            [ (TxMetaNumber 1, TxMetaNumber 2)
            , (TxMetaNumber 11, TxMetaNumber 3)
            , (TxMetaBytes "A", TxMetaBytes "B")
            , (TxMetaText "a", TxMetaText "b")
            , (TxMetaBytes "0A", TxMetaBytes "E")
            , (TxMetaText "aa", TxMetaText "bb")
            , (TxMetaText "ab", TxMetaText "ba")
            , (TxMetaBytes "\DLE@A", TxMetaBytes "C")
            , (TxMetaBytes "\NAK@A", TxMetaBytes "D")
            , (TxMetaText "aab", TxMetaText "ba")
            ,
              ( TxMetaText "aba"
              , TxMetaMap
                  [ (TxMetaNumber 1, TxMetaNumber 2)
                  , (TxMetaNumber 11, TxMetaNumber 3)
                  , (TxMetaBytes "A", TxMetaBytes "B")
                  , (TxMetaText "a", TxMetaText "b")
                  , (TxMetaBytes "0A", TxMetaBytes "E")
                  , (TxMetaText "aa", TxMetaText "bb")
                  , (TxMetaText "ab", TxMetaText "ba")
                  , (TxMetaBytes "\DLE@A", TxMetaBytes "C")
                  , (TxMetaBytes "\NAK@A", TxMetaBytes "D")
                  , (TxMetaText "aab", TxMetaText "ba")
                  , (TxMetaText "abb", TxMetaText "ba")
                  ]
              )
            , (TxMetaText "abb", TxMetaText "ba")
            ]
        )
    )

prop_golden_9 :: Property
prop_golden_9 =
  matchMetadata
    TxMetadataJsonDetailedSchema
    [r|{"0":
      {"map":
        [ { "k": {"string": "aaa"}
          , "v": {"string": "b4"}
          }
        , { "k": {"int": 1}
          , "v": {"string": "b6"}
          }
        , { "k": {"string": "aa"}
          , "v": {"string": "b2"}
          }
        , { "k": {"string": "ab"}
          , "v": {"string": "b3"}
          }
        , { "k": {"string": "b"}
          , "v": {"string": "b5"}
          }
        , { "k": {"string": "a"}
          , "v": {"string": "b1"}
          }
        ]
      }}|]
    ( txMetadataSingleton
        0
        ( TxMetaMap
            [ (TxMetaText "aaa", TxMetaText "b4")
            , (TxMetaNumber 1, TxMetaText "b6")
            , (TxMetaText "aa", TxMetaText "b2")
            , (TxMetaText "ab", TxMetaText "b3")
            , (TxMetaText "b", TxMetaText "b5")
            , (TxMetaText "a", TxMetaText "b1")
            ]
        )
    )

txMetadataSingleton :: Word64 -> TxMetadataValue -> TxMetadata
txMetadataSingleton n v = TxMetadata (fromList [(n, v)])

matchMetadata
  :: HasCallStack
  => TxMetadataJsonSchema
  -> ByteString
  -- ^ json string to test
  -> TxMetadata
  -- ^ expected metadata
  -> Property
matchMetadata hasSchema jsonStr expectedMetadata = withFrozenCallStack $ H.propertyOnce $ do
  json <- H.noteShowM . H.nothingFail $ Aeson.decodeStrict' jsonStr
  metadata <- H.noteShowM . H.leftFail $ metadataFromJson hasSchema json
  metadata === expectedMetadata

-- ----------------------------------------------------------------------------
-- Round trip properties
--

-- | Any JSON (within the supported subset) can be converted to tx metadata and
-- back, to give the same original JSON.
--
-- This uses the \"no schema\" mapping. Note that with this mapping it is /not/
-- the case that any tx metadata can be converted to JSON and back to give the
-- original value.
prop_noschema_json_roundtrip_via_metadata :: Property
prop_noschema_json_roundtrip_via_metadata = H.property $ do
  json <- H.forAll (genJsonForTxMetadata TxMetadataJsonNoSchema)
  Right json
    === ( fmap (metadataToJson TxMetadataJsonNoSchema)
            . metadataFromJson TxMetadataJsonNoSchema
        )
      json

-- | Any JSON (fitting the detailed schema) can be converted to tx metadata and
-- back, to give the same original JSON.
prop_schema_json_roundtrip_via_metadata :: Property
prop_schema_json_roundtrip_via_metadata = H.property $ do
  json <- H.forAll (genJsonForTxMetadata TxMetadataJsonDetailedSchema)
  Right json
    === ( fmap (metadataToJson TxMetadataJsonDetailedSchema)
            . metadataFromJson TxMetadataJsonDetailedSchema
        )
      json

-- | Any tx metadata can be converted to JSON (using the detailed schema) and
-- back, to give the same original tx metadata.
prop_metadata_roundtrip_via_schema_json :: Property
prop_metadata_roundtrip_via_schema_json = H.property $ do
  md <- H.forAll genTxMetadata
  Right md
    === ( metadataFromJson TxMetadataJsonDetailedSchema
            . metadataToJson TxMetadataJsonDetailedSchema
        )
      md

prop_metadata_chunks
  :: (Show str, Eq str, Monoid str)
  => Gen str
  -> (str -> TxMetadataValue)
  -> (TxMetadataValue -> Maybe str)
  -> Property
prop_metadata_chunks genStr toMetadataValue extractChunk = H.property $ do
  str <- H.forAll genStr
  case toMetadataValue str of
    metadataValue@(TxMetaList chunks) -> do
      H.cover 1 "Empty chunks" (null chunks)
      H.cover 5 "Single chunks" (length chunks == 1)
      H.cover 25 "Many chunks" (length chunks > 1)
      str === mconcat (mapMaybe extractChunk chunks)
      Right () === validateTxMetadata metadata
     where
      metadata = makeTransactionMetadata (Map.singleton 0 metadataValue)
    _ ->
      H.failure

prop_metadata_text_chunks :: Property
prop_metadata_text_chunks =
  prop_metadata_chunks
    (Gen.text (Range.linear 0 255) Gen.unicodeAll)
    metaTextChunks
    ( \case
        TxMetaText chunk -> Just chunk
        _ -> Nothing
    )

prop_metadata_bytes_chunks :: Property
prop_metadata_bytes_chunks =
  prop_metadata_chunks
    (Gen.bytes (Range.linear 0 255))
    metaBytesChunks
    ( \case
        TxMetaBytes chunk -> Just chunk
        _ -> Nothing
    )

-- ----------------------------------------------------------------------------
-- Automagically collecting all the tests
--

tests :: TestTree
tests =
  testGroup
    "Test.Cardano.Api.Metadata"
    [ testProperty "golden 1" prop_golden_1
    , testProperty "golden 2" prop_golden_2
    , testProperty "golden 3" prop_golden_3
    , testProperty "golden 4" prop_golden_4
    , testProperty "golden 5" prop_golden_5
    , testProperty "golden 6" prop_golden_6
    , testProperty "golden 7" prop_golden_7
    , testProperty "golden 8" prop_golden_8
    , testProperty "golden 9" prop_golden_9
    , testProperty "noschema json roundtrip via metadata" prop_noschema_json_roundtrip_via_metadata
    , testProperty "schema json roundtrip via metadata" prop_schema_json_roundtrip_via_metadata
    , testProperty "metadata roundtrip via schema json" prop_metadata_roundtrip_via_schema_json
    , testProperty "valid & rountrip text chunks" prop_metadata_text_chunks
    , testProperty "valid & rountrip bytes chunks" prop_metadata_bytes_chunks
    ]
