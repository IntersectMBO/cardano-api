{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Hedgehog.Roundtrip.CBOR
  ( decodeOnlyPlutusScriptBytes
  , trippingCbor
  )
where

import           Cardano.Api
import           Cardano.Api.Eon.ShelleyBasedEra
import           Cardano.Api.Script

import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Plutus.Language as Plutus

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Short as SBS
import           Data.Proxy
import           GHC.Stack (HasCallStack)
import qualified GHC.Stack as GHC

import qualified Hedgehog as H
import           Hedgehog.Internal.Property (failWith)

{- HLINT ignore "Use camelCase" -}

-- | Assert that CBOR serialisation and deserialisation roundtrips.
trippingCbor
  :: ()
  => HasCallStack
  => H.MonadTest m
  => Show a
  => Eq a
  => SerialiseAsCBOR a
  => AsType a
  -> a
  -> m ()
trippingCbor typeProxy v =
  GHC.withFrozenCallStack $
    H.tripping v serialiseToCBOR (deserialiseFromCBOR typeProxy)

-- | We need to confirm the existing 'SerialiseAsCBOR' instance for 'Script lang'
-- no longer double serializes scripts but is backwards compatible with
-- doubly serialized scripts.
--
-- We would also like to check that the deserialized bytes is a valid
-- plutus script. We can do this by using the 'SerialiseAsCBOR' instance for
-- 'PlutusScriptInEra'.
--
-- We will check the following:
-- 1. Deserializing double encoded script bytes and "normal" script bytes
--    decode to the same byte sequence.
-- 2. The resulting bytes are both valid plutus scripts (via 'PlutusScriptInEra')
--
-- If these two properties hold we can be sure that existing double encoded scripts
-- will deserialize correctly and newly created scripts will also deserialize correctly.
decodeOnlyPlutusScriptBytes
  :: forall era lang m
   . HasCallStack
  => Ledger.Era (ShelleyLedgerEra era)
  => H.MonadTest m
  => Plutus.PlutusLanguage (ToLedgerPlutusLanguage lang)
  => IsPlutusScriptLanguage lang
  => HasTypeProxy era
  => ShelleyBasedEra era
  -> PlutusScriptVersion lang
  -> ByteString
  -- ^ This can be a double encoded or "normal" plutus script
  -> AsType (Script lang)
  -> m ()
decodeOnlyPlutusScriptBytes _ _ scriptBytes typeProxy = do
  -- Decode a plutus script (double wrapped or "normal" plutus script) with the existing SerialiseAsCBOR instance for
  -- 'Script lang'. This should produce plutus script bytes that are not double encoded.
  (PlutusScriptSerialised expectedToBeValidScriptBytes) <- case deserialiseFromCBOR typeProxy scriptBytes of
    Left e -> failWith Nothing $ "Plutus lang: Error decoding script bytes: " ++ show e
    Right (SimpleScript _) -> failWith Nothing "Simple script found. Should be impossible."
    Right (PlutusScript _ plutusScript) -> return plutusScript

  -- We check that the script is "runnable" and of the expected language via the
  -- 'PlutusScriptInEra era lang' SerialiseAsCBOR instance.
  (PlutusScriptSerialised confirmedToBeValidScriptBytes) <-
    case deserialiseFromCBOR (AsPlutusScriptInEra @era (proxyToAsType (Proxy :: Proxy lang))) $
      SBS.fromShort expectedToBeValidScriptBytes of
      Left e -> failWith Nothing $ "PlutusScriptInEra: Error decoding plutus script bytes: " ++ show e
      Right (PlutusScriptInEra p) -> return p

  -- We also confirm that PlutusScriptInEra SerialiseAsCBOR instance can handle double encoded
  -- plutus scripts.
  case deserialiseFromCBOR (AsPlutusScriptInEra @era (proxyToAsType (Proxy :: Proxy lang))) scriptBytes of
    Left e -> failWith Nothing $ "PlutusScriptInEra: Error decoding double wrapped bytes: " ++ show e
    Right (PlutusScriptInEra (PlutusScriptSerialised shouldAlsoBeValidScriptBytes)) -> do
      confirmedToBeValidScriptBytes H.=== shouldAlsoBeValidScriptBytes

  -- If we have fixed the double encoding issue, the bytes produced
  -- should be the same.
  expectedToBeValidScriptBytes H.=== confirmedToBeValidScriptBytes
