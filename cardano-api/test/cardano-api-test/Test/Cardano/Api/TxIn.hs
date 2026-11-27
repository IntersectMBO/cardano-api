module Test.Cardano.Api.TxIn
  ( tests
  )
where

import Cardano.Api
import Cardano.Api.Parser.Text qualified as P

import Data.List qualified as List

import Test.Gen.Cardano.Api.Typed (genTxIn)

import Hedgehog as H
import Hedgehog.Extras qualified as H
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "Test.Cardano.Api.TxIn"
    [ testProperty "roundtrip TxIn render and parse" prop_roundtrip_txin_render_parse
    , testProperty "parse TxIn with the largest index" prop_parse_txin_max_index
    , testProperty "parse TxIn rejects index 65536" prop_parse_txin_rejects_index_above_max
    , testProperty "parse TxIn rejects index 2^64+5" prop_parse_txin_rejects_index_above_word64
    ]

prop_roundtrip_txin_render_parse :: Property
prop_roundtrip_txin_render_parse = H.property $ do
  txIn <- H.forAll genTxIn
  H.tripping txIn renderTxIn $ P.runParser parseTxIn

prop_parse_txin_max_index :: Property
prop_parse_txin_max_index = H.propertyOnce $ do
  TxIn _ txIx <- H.evalEither . P.runParser parseTxIn $ exampleTxId <> "#65535"
  txIx === TxIx 65535

prop_parse_txin_rejects_index_above_max :: Property
prop_parse_txin_rejects_index_above_max = H.propertyOnce $ assertTxIxRejected 65536

-- | 2^64 + 5 used to wrap to index 5.
prop_parse_txin_rejects_index_above_word64 :: Property
prop_parse_txin_rejects_index_above_word64 =
  H.propertyOnce $ assertTxIxRejected 18446744073709551621

-- | Assert that parsing a 'TxIn' with the given index fails and that the error names the index.
assertTxIxRejected
  :: H.MonadTest m
  => Integer
  -- ^ The transaction index to parse
  -> m ()
assertTxIxRejected ix =
  H.assertWith (P.runParser parseTxIn $ exampleTxId <> "#" <> textShow ix) $
    either (show ix `List.isInfixOf`) (const False)

exampleTxId :: Text
exampleTxId = "be6efd42a3d7b9a00d09d77a5d41e55ceaf0bd093a8aa8a893ce70d9caafd978"
