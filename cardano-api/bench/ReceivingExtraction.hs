{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}

-- Measure the actual public indexed-witness extraction consumer. Reference
-- metadata is synthetic: no script resolution or ledger validation is timed.
module Main (main) where

import Cardano.Api qualified as Api
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.AnyScriptWitness qualified as Exp
import Cardano.Api.Experimental.Plutus qualified as Exp
import Cardano.Api.Experimental.Tx qualified as Exp
import Cardano.Api.Ledger qualified as L

import Cardano.Ledger.Core qualified as Core
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Scripts qualified as DS
import Cardano.Ledger.Dijkstra.TxBody qualified as Dijkstra
import Cardano.Ledger.Mary.Value qualified as Mary

import Control.Exception (evaluate)
import Control.Monad (forM_, replicateM, unless)
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import GHC.Exts (fromList)
import Lens.Micro
import System.CPUTime (getCPUTime)
import System.Mem (performGC)
import Text.Printf (printf)

main :: IO ()
main = do
  txId <-
    either (fail . show) pure $
      Api.deserialiseFromRawBytesHex
        "0000000000000000000000000000000000000000000000000000000000000000"
  let input = Api.TxIn txId (Api.TxIx 0)
  forM_ [200, 400, 800] $ \size -> do
    let hashes = sort [scriptHash seed | seed <- [0 .. size - 1]]
        content = fixture input hashes
        expected = sum [fromIntegral index + 1 | index <- [1, 3 .. size - 1]] + fromIntegral size + 1
        body =
          (Core.mkBasicTxBody :: L.TxBody L.TopTx DijkstraEra)
            & L.outputsTxBodyL .~ fromList [out | Exp.TxOut out <- Exp.txOuts content]
    unless (length (Dijkstra.receivingScriptTargets body) == size + 1) $
      fail "setup: per-output targets lost a duplicate hash"
    actual <- evaluate $ extractChecksum 0 content
    unless (actual == expected) $
      fail "setup: raw native gaps or duplicate-output Receiving indices differ"
    forM_ [1 .. 10] $ \salt -> do
      _ <- evaluate $ extractChecksum salt content
      pure ()
    performGC
    start <- getCPUTime
    checksums <- replicateM 500 $ do
      salt <- getCPUTime
      evaluate $ extractChecksum salt content
    end <- getCPUTime
    unless (all (== expected) checksums) $ fail "timed extraction changed the Receiving pointers"
    printf
      "distinct_hashes=%d plutus_outputs=%d iterations=500 cpu_ms=%.3f checksum=%d\n"
      size
      (size `div` 2 + 1)
      (fromIntegral (end - start) / 1e9 :: Double)
      actual

scriptHash :: Int -> Core.ScriptHash
scriptHash seed =
  Core.hashScript (Core.fromNativeScript native :: L.Script DijkstraEra)
 where
  native =
    DS.upgradeTimelock $ Api.toAllegraTimelock $ Api.RequireTimeAfter $ Api.SlotNo $ fromIntegral seed

fixture :: Api.TxIn -> [Core.ScriptHash] -> Exp.TxBodyContent DijkstraEra
fixture input hashes =
  Exp.defaultTxBodyContent
    & Exp.setTxOuts (map (Exp.TxOut . output) $ hashes ++ take 1 (drop 1 hashes))
    & Exp.setTxReceivingWitnesses
      ( Map.fromList $
          [(fromIntegral outputIndex, witness) | outputIndex <- [1, 3 .. length hashes - 1]]
            ++ [(fromIntegral $ length hashes, witness)]
      )
 where
  output hash =
    L.mkBasicTxOut
      (Core.AddrProtected L.Testnet (L.ScriptHashObj hash) L.StakeRefNull)
      (Mary.MaryValue (L.Coin 3_000_000) mempty)
  witness =
    Exp.AnyScriptWitnessPlutus $
      Exp.AnyPlutusReceivingScriptWitness $
        Exp.PlutusScriptWitness
          L.SPlutusV4
          (Exp.PReferenceScript input)
          Exp.NoScriptDatum
          (Api.unsafeHashableScriptData $ Api.ScriptDataNumber 0)
          (Api.ExecutionUnits 0 0)

-- Salt changes a non-domain output field so each call evaluates the public
-- consumer afresh. Timed calls include this preparation cost.
{-# NOINLINE extractChecksum #-}
extractChecksum :: Integer -> Exp.TxBodyContent DijkstraEra -> Word64
extractChecksum salt content =
  case Exp.extractAllIndexedPlutusScriptWitnesses Exp.DijkstraEra salted of
    Left err -> error $ show err
    Right indexed ->
      sum
        [ fromIntegral index + 1
        | Exp.AnyIndexedPlutusScriptWitness
            (Exp.IndexedPlutusScriptWitness (Exp.WitReceiving _ _) (L.DijkstraReceiving (L.AsIx index)) _) <-
            indexed
        ]
 where
  salted =
    content
      & Exp.setTxOuts
        [ Exp.TxOut $ out & L.coinTxOutL .~ L.Coin (3_000_000 + salt `mod` 10)
        | Exp.TxOut out <- Exp.txOuts content
        ]
