{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
module Test.Cardano.Api.Experimental
  ( tests
  )
where

import Test.Tasty (TestTree, testGroup)
import Hedgehog (Property, success)
import Test.Tasty.Hedgehog (testProperty)
import Hedgehog.Extras (propertyOnce)
import qualified Cardano.Api as Api
import qualified Cardano.Api.Experimental as Exp
import Control.Monad.IO.Class (liftIO)
import Lens.Micro ((&))
import Cardano.Api.Tx.Body ( setTxIns, setTxFee, setTxOuts )
import Cardano.Api (TxFee(TxFeeExplicit))
import Cardano.Api.Ledger (Coin(Coin))

-- | Tests in this module can be run by themselves by writing:
-- ```bash
-- cabal test cardano-api-test --test-options="--pattern=Test.Cardano.Api.Experimental"
-- ```

tests :: TestTree
tests =
  testGroup
    "Test.Cardano.Api.Experimental"
    [ testProperty "Create transaction with experimental API" prop_create_transaction_with_experimental_api
    ]

prop_create_transaction_with_experimental_api :: Property
prop_create_transaction_with_experimental_api = propertyOnce $ do
  let era = Exp.ConwayEra
  let sbe = Api.convert era
  let txBodyContent = Api.defaultTxBodyContent sbe
                        & setTxIns []
                        & setTxOuts []
                        & setTxFee (TxFeeExplicit sbe (Coin 2_000_000))

  let unsignedTx = Exp.makeUnsignedTx era txBodyContent

  case unsignedTx of
    Left e -> liftIO $ putStrLn $ "Error: " <> show e
    Right tx -> do
      let bootstrapWitnesses = []
          keyWitnesses = []
      let signedTx = Exp.signTx era bootstrapWitnesses keyWitnesses tx
      liftIO $ print signedTx
  
  success
