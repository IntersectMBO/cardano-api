{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Test.Cardano.Api.Transaction.Utils
  ( mkSimpleUTxOs
  , loadPlutusWitness
  , textEnvTypes
  , mkUtxos
  , mkAddress
  , mkTxOutput
  , parseSystemStart
  , getTxOutCoin
  , mkCredential
  , mkTxIn
  )
where

import Cardano.Api
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Parser.Text qualified as P

import Cardano.Ledger.Alonzo.Core qualified as L
import Cardano.Ledger.Credential qualified as L
import Cardano.Ledger.Mary.Value qualified as L

import Control.Monad.Trans.Fail (errorFail)
import Data.ByteString qualified as B
import Data.Maybe
import Data.Time.Format qualified as DT
import GHC.Exts (IsList (..))
import GHC.Stack

import Test.Cardano.Api.Orphans ()

import Hedgehog (MonadTest)
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H

mkSimpleUTxOs :: ShelleyBasedEra ConwayEra -> UTxO ConwayEra
mkSimpleUTxOs sbe =
  UTxO
    [
      ( mkTxIn "01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53#0"
      , TxOut
          ( AddressInEra
              (ShelleyAddressInEra sbe)
              ( ShelleyAddress
                  L.Testnet
                  (mkCredential "keyHash-ebe9de78a37f84cc819c0669791aa0474d4f0a764e54b9f90cfe2137")
                  L.StakeRefNull
              )
          )
          ( lovelaceToTxOutValue
              sbe
              2_000_000_000
          )
          TxOutDatumNone
          ReferenceScriptNone
      )
    ]

loadPlutusWitness
  :: HasCallStack
  => MonadFail m
  => MonadIO m
  => MonadTest m
  => ConwayEraOnwards era
  -> m (ScriptHash, ScriptWitness WitCtxMint era)
loadPlutusWitness ceo = do
  envelope <-
    H.leftFailM $
      fmap (deserialiseFromJSON @TextEnvelope) . H.evalIO $
        B.readFile "test/cardano-api-test/files/input/plutus/v3.alwaysTrue.json"
  ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV3) s@(PlutusScript PlutusScriptV3 script) <-
    H.leftFail $ deserialiseFromTextEnvelopeAnyOf textEnvTypes envelope
  let scriptLangInEra = case ceo of
        ConwayEraOnwardsConway -> PlutusScriptV3InConway
        ConwayEraOnwardsDijkstra -> PlutusScriptV3InDijkstra
  pure
    ( hashScript s
    , PlutusScriptWitness
        scriptLangInEra
        PlutusScriptV3
        (PScript script)
        NoScriptDatumForMint
        (unsafeHashableScriptData (ScriptDataMap []))
        (ExecutionUnits 0 0)
    )

textEnvTypes :: [FromSomeType HasTextEnvelope ScriptInAnyLang]
textEnvTypes =
  [ FromSomeType
      (AsScript AsPlutusScriptV3)
      (ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV3))
  ]

mkUtxos
  :: BabbageEraOnwards era
  -> Maybe L.ScriptHash
  -- ^ add an asset to the utxo if the script hash is provided
  -> UTxO era
mkUtxos beo mScriptHash = babbageEraOnwardsConstraints beo $ do
  let sbe = convert beo
  UTxO
    [
      ( mkTxIn "01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53#0"
      , TxOut
          ( AddressInEra
              (ShelleyAddressInEra sbe)
              ( ShelleyAddress
                  L.Testnet
                  (mkCredential "keyHash-ebe9de78a37f84cc819c0669791aa0474d4f0a764e54b9f90cfe2137")
                  L.StakeRefNull
              )
          )
          ( TxOutValueShelleyBased
              sbe
              ( L.MaryValue
                  (L.Coin 4_000_000)
                  ( L.MultiAsset $
                      fromList
                        [(L.PolicyID scriptHash, [(L.AssetName "eeee", 1)]) | scriptHash <- maybeToList mScriptHash]
                  )
              )
          )
          TxOutDatumNone
          ReferenceScriptNone
      )
    ]

-- | Make an address from a script hash
mkAddress :: ShelleyBasedEra era -> L.ScriptHash -> AddressInEra era
mkAddress sbe scriptHash =
  AddressInEra
    (ShelleyAddressInEra sbe)
    ( ShelleyAddress
        L.Testnet
        (L.ScriptHashObj scriptHash)
        L.StakeRefNull
    )

-- | Make a single txout with an optional asset
mkTxOutput
  :: BabbageEraOnwards era
  -> AddressInEra era
  -> L.Coin
  -- ^ output ADA
  -> Maybe L.ScriptHash
  -- ^ there will be an asset in the txout if provided
  -> [TxOut CtxTx era]
mkTxOutput beo address coin mScriptHash = babbageEraOnwardsConstraints beo $ do
  let sbe = convert beo
  [ TxOut
      address
      ( TxOutValueShelleyBased
          sbe
          ( L.MaryValue
              coin
              ( L.MultiAsset $
                  fromList
                    [(L.PolicyID scriptHash, [(L.AssetName "eeee", 2)]) | scriptHash <- maybeToList mScriptHash]
              )
          )
      )
      TxOutDatumNone
      ReferenceScriptNone
    ]

parseSystemStart :: (HasCallStack, MonadTest m, MonadIO m) => String -> m SystemStart
parseSystemStart timeString =
  withFrozenCallStack $
    fmap SystemStart . H.evalIO $
      DT.parseTimeM True DT.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" timeString

getTxOutCoin
  :: forall era ctx m
   . (HasCallStack, MonadFail m, IsMaryBasedEra era)
  => TxOut ctx era
  -> m L.Coin
getTxOutCoin txout = withFrozenCallStack $ maryEraOnwardsConstraints (maryBasedEra @era) $ do
  TxOut _ (TxOutValueShelleyBased _ (L.MaryValue changeCoin _)) _ _ <- pure txout
  pure changeCoin

mkCredential :: HasCallStack => Text -> L.Credential k
mkCredential = errorFail @String . L.parseCredential

mkTxIn :: HasCallStack => Text -> TxIn
mkTxIn = either error id . P.runParser parseTxIn
