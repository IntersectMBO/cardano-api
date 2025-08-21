module Main where

import Cardano.Crypto.Libsodium (sodiumInit)

import System.IO (BufferMode (LineBuffering), hSetBuffering, hSetEncoding, stdout, utf8)

import Test.Gen.Cardano.Api.Byron qualified

import Test.Cardano.Api.Address qualified
import Test.Cardano.Api.Bech32 qualified
import Test.Cardano.Api.CBOR qualified
import Test.Cardano.Api.Cip129 qualified
import Test.Cardano.Api.Crypto qualified
import Test.Cardano.Api.Envelope qualified
import Test.Cardano.Api.EpochLeadership qualified
import Test.Cardano.Api.Eras qualified
import Test.Cardano.Api.Experimental qualified
import Test.Cardano.Api.Genesis qualified
import Test.Cardano.Api.GovAnchorValidation qualified
import Test.Cardano.Api.IO qualified
import Test.Cardano.Api.Json qualified
import Test.Cardano.Api.KeysByron qualified
import Test.Cardano.Api.Ledger qualified
import Test.Cardano.Api.Metadata qualified
import Test.Cardano.Api.Ord qualified
import Test.Cardano.Api.RawBytes qualified
import Test.Cardano.Api.Transaction.Autobalance qualified
import Test.Cardano.Api.Transaction.Body.Plutus.Scripts qualified
import Test.Cardano.Api.TxBody qualified
import Test.Cardano.Api.Value qualified

import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = do
  -- TODO: Remove sodiumInit: https://github.com/input-output-hk/cardano-base/issues/175
  sodiumInit
  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
  defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Cardano.Api"
    [ Test.Gen.Cardano.Api.Byron.tests
    , Test.Cardano.Api.Address.tests
    , Test.Cardano.Api.Bech32.tests
    , Test.Cardano.Api.CBOR.tests
    , Test.Cardano.Api.Cip129.tests
    , Test.Cardano.Api.Crypto.tests
    , Test.Cardano.Api.GovAnchorValidation.tests
    , Test.Cardano.Api.Envelope.tests
    , Test.Cardano.Api.EpochLeadership.tests
    , Test.Cardano.Api.Eras.tests
    , Test.Cardano.Api.Experimental.tests
    , Test.Cardano.Api.Genesis.tests
    , Test.Cardano.Api.IO.tests
    , Test.Cardano.Api.Json.tests
    , Test.Cardano.Api.KeysByron.tests
    , Test.Cardano.Api.Ledger.tests
    , Test.Cardano.Api.Metadata.tests
    , Test.Cardano.Api.Ord.tests
    , Test.Cardano.Api.RawBytes.tests
    , Test.Cardano.Api.Transaction.Body.Plutus.Scripts.tests
    , Test.Cardano.Api.Transaction.Autobalance.tests
    , Test.Cardano.Api.TxBody.tests
    , Test.Cardano.Api.Value.tests
    ]
