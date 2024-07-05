{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.Api.Scripts.New where

import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Protocol.AvailableEras
import           Cardano.Api.SerialiseCBOR (SerialiseAsCBOR (..))

import qualified Cardano.Binary as CBOR
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import qualified Cardano.Ledger.Babbage as Ledger
import           Cardano.Ledger.Binary
import qualified Cardano.Ledger.Binary.Plain as Plain
import qualified Cardano.Ledger.Conway as Ledger
import qualified Cardano.Ledger.Core as Ledger

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import           Data.Typeable

{-
In the current api we have PlutusScript which is a wrapper around ShortByteString
We should instead use newtype Plutus from ledger which is the same thing but
there is exposed functionality.
There is mkPlutusScript which will give us the different available scripts
for a given era (if they are available)

We are going to start with the assumption that the user will know what
script version they are using and will indicate as such.

I.e we will be removing ScriptInAnyLang because it depends on
the text envelope format and doesn't actually check the
script bytes.

We have an issue regarding what is and isn't available in the various eras

We can actually stick to the latest era and the upcoming era. We depend
on ledger to get the available script versions correctly so we should
be able to avoid parameterizing on versions.

-}

-- | This type wraps the serialized scripts (native or plutus)
-- for a given era. However we want to restrict the era to the
-- latest era (mainnet) and the upcoming era. This removes
-- the complexity of deciding which scripts are available in which eras.

newtype Script (availableera :: AvailableEras)
  = Script { unScript :: Ledger.Script (ToConstrainedEra availableera) }

instance ( HasTypeProxy (Script availableera)
         , ToCBOR (Ledger.Script (ToConstrainedEra availableera))
         , Typeable availableera
         , ConstrainedDecoder availableera ledgerera
         , Ledger.Era ledgerera
         , DecCBOR (Ledger.Script (ToConstrainedEra availableera)))
         => SerialiseAsCBOR (Script availableera) where
  serialiseToCBOR (Script s) = CBOR.serialize' s

  deserialiseFromCBOR _ bs =
    Plain.decodeFullDecoder
      "Script"
      fromCBOR
      (LBS.fromStrict bs) :: Either DecoderError (Script availableera)

instance HasTypeProxy (Script BabbageEra) where
  data AsType (Script BabbageEra) = AsMainnetScript
  proxyToAsType :: Proxy (Script BabbageEra) -> AsType (Script BabbageEra)
  proxyToAsType _ = AsMainnetScript

instance HasTypeProxy (Script ConwayEra) where
  data AsType (Script ConwayEra) = AsUpcomingEraScript
  proxyToAsType :: Proxy (Script ConwayEra) -> AsType (Script ConwayEra)
  proxyToAsType _ = AsUpcomingEraScript

instance ( Typeable availableera
         , Ledger.Era ledgerera
         , DecCBOR (Ledger.Script (ToConstrainedEra availableera))
         , ConstrainedDecoder availableera ledgerera
         ) => FromCBOR (Script availableera) where
  fromCBOR = Script <$> fromEraCBORConstrained @availableera

class ConstrainedDecoder (availableera :: AvailableEras) era | availableera -> era where
  fromEraCBORConstrained :: (Ledger.Era era, DecCBOR t) => Plain.Decoder s t

instance ConstrainedDecoder BabbageEra Ledger.Babbage where
  fromEraCBORConstrained = Ledger.fromEraCBOR @Ledger.Babbage

instance ConstrainedDecoder ConwayEra Ledger.Conway where
  fromEraCBORConstrained = Ledger.fromEraCBOR @Ledger.Conway

-- You need a function that lets a user decode a script in a given era
-- The function must only try to decode script versions available in a given era
-- Can we create a type class that enforces the behavior? Or a type family?
{-
data DeserializationError
deserialiseNativeScript
  :: AvailableEras
  -> ByteString
  -> Either DeserializationError (NativeScript (ToConstrainedEra availableera))
  -}

data NativeScriptDeserializationError
  = NotAScript DecoderError
  | NotASimpleScript -- We can improve this and potentially
                     -- tell the consumer its a plutus script
                     -- and which version it is.


deserialiseNativeScript
  :: DecCBOR (Ledger.AlonzoScript (ToConstrainedEra availableera))
  => Era availableera
  -> ByteString
  -> Either NativeScriptDeserializationError (Ledger.NativeScript (ToConstrainedEra availableera))
deserialiseNativeScript availableEra  bs =
  case availableEra of
    CurrentEraInternal -> deserialise AsMainnetScript bs
    UpcomingEraInternal -> deserialise AsUpcomingEraScript bs
  where
   deserialise
     :: SerialiseAsCBOR (Script availableera)
     => Ledger.EraScript (ToConstrainedEra availableera)
     => AsType (Script availableera)
     -> ByteString
     -> Either NativeScriptDeserializationError (Ledger.NativeScript (ToConstrainedEra availableera))
   deserialise as bs' =
     case deserialiseFromCBOR as bs' of
       Right s -> case Ledger.getNativeScript $ unScript s of
                    Just nScript -> Right nScript
                    Nothing -> Left NotASimpleScript
       Left e -> Left $ NotAScript e
