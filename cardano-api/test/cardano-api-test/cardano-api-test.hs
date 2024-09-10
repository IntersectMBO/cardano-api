module Main where

import           Cardano.Crypto.Libsodium (sodiumInit)

import           System.IO (BufferMode (LineBuffering), hSetBuffering, hSetEncoding, stdout, utf8)

import qualified Test.Gen.Cardano.Api.Byron

import qualified Test.Cardano.Api.Address
import qualified Test.Cardano.Api.Bech32
import qualified Test.Cardano.Api.CBOR
import qualified Test.Cardano.Api.Crypto
import qualified Test.Cardano.Api.Envelope
import qualified Test.Cardano.Api.EpochLeadership
import qualified Test.Cardano.Api.Eras
import qualified Test.Cardano.Api.Genesis
import qualified Test.Cardano.Api.IO
import qualified Test.Cardano.Api.Json
import qualified Test.Cardano.Api.KeysByron
import qualified Test.Cardano.Api.Ledger
import qualified Test.Cardano.Api.Metadata
import qualified Test.Cardano.Api.Ord
import qualified Test.Cardano.Api.ProtocolParameters
import qualified Test.Cardano.Api.RawBytes
import qualified Test.Cardano.Api.Transaction.Autobalance
import qualified Test.Cardano.Api.TxBody
import qualified Test.Cardano.Api.Value

import           Test.Tasty (TestTree, defaultMain, testGroup)

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
    , Test.Cardano.Api.Crypto.tests
    , Test.Cardano.Api.Envelope.tests
    , Test.Cardano.Api.EpochLeadership.tests
    , Test.Cardano.Api.Eras.tests
    , Test.Cardano.Api.Genesis.tests
    , Test.Cardano.Api.IO.tests
    , Test.Cardano.Api.Json.tests
    , Test.Cardano.Api.KeysByron.tests
    , Test.Cardano.Api.Ledger.tests
    , Test.Cardano.Api.Metadata.tests
    , Test.Cardano.Api.Ord.tests
    , Test.Cardano.Api.ProtocolParameters.tests
    , Test.Cardano.Api.RawBytes.tests
    , Test.Cardano.Api.Transaction.Autobalance.tests
    , Test.Cardano.Api.TxBody.tests
    , Test.Cardano.Api.Value.tests
    ]
