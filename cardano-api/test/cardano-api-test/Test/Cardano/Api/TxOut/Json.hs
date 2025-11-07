{-# LANGUAGE OverloadedStrings #-}

-- | Comprehensive JSON tests for TxOut instances
--
-- This module provides extensive testing coverage for the ToJSON and FromJSON
-- instances of TxOut across all eras and contexts.
--
-- Test coverage includes:
-- - Roundtrip tests for all eras (Byron through Dijkstra)
-- - Both CtxTx and CtxUTxO contexts
-- - All datum types (None, Hash, Supplemental, Inline)
-- - Error cases (conflicting fields, mismatched hashes, etc.)
-- - Edge cases (null handling, supplemental datum ambiguity)
-- - ToJSON output validation
module Test.Cardano.Api.TxOut.Json
  ( tests
  )
where

import Test.Cardano.Api.TxOut.JsonEdgeCases qualified as EdgeCases
import Test.Cardano.Api.TxOut.JsonErrorCases qualified as ErrorCases
import Test.Cardano.Api.TxOut.JsonRoundtrip qualified as Roundtrip

import Test.Tasty (TestTree, testGroup)

-- | All TxOut JSON tests
tests :: TestTree
tests =
  testGroup
    "TxOut.Json"
    [ Roundtrip.tests
    , ErrorCases.tests
    , EdgeCases.tests
    ]
