module Cardano.Api.Key
  ( -- * The Key class
    Key (..)
  , generateSigningKey
  , generateInsecureSigningKey
  , CastVerificationKeyRole (..)
  , CastSigningKeyRole (..)
  , AsType (AsVerificationKey, AsSigningKey)

    -- * Hash
  , Hash (..)
  , CastHash (..)
  , renderSafeHashAsHex
  , parseHexHash

    -- * Key mnemonic
  , MnemonicSize (..)
  , generateMnemonic
  , MnemonicToSigningKeyError (..)
  , signingKeyFromMnemonic
  , signingKeyFromMnemonicWithPaymentKeyIndex
  , findMnemonicWordsWithPrefix
  , autocompleteMnemonicPrefix

    -- * Main Key types
  , CommitteeColdKey
  , CommitteeColdExtendedKey
  , CommitteeHotKey
  , CommitteeHotExtendedKey
  , DRepKey
  , DRepExtendedKey
  , PaymentKey
  , PaymentExtendedKey
  , StakeKey
  , StakeExtendedKey
  , StakePoolExtendedKey
  , StakePoolKey
  , GenesisKey
  , GenesisExtendedKey
  , GenesisDelegateKey
  , GenesisDelegateExtendedKey
  , GenesisUTxOKey

    -- ** Data family instances
  , VerificationKey (..)
  , SigningKey (..)
  , AnyStakePoolVerificationKey (..)
  , anyStakePoolVerificationKeyHash
  , AnyStakePoolSigningKey (..)
  , anyStakePoolSigningKeyToVerificationKey

    -- * Verification key sum type
  , SomeAddressVerificationKey (..)
  , deserialiseAnyVerificationKey
  , deserialiseAnyVerificationKeyBech32
  , deserialiseAnyVerificationKeyTextEnvelope
  , renderSomeAddressVerificationKey
  , mapSomeAddressVerificationKey

    -- * Praos consensus

    -- | Praos consensus key types and their 'Key' class instances

    -- ** Key types
  , KesKey
  , VrfKey

    -- ** Signing
  , signArbitraryBytesKes

    -- ** Type proxy
  , HasTypeProxy (..)
  , asType
  , Proxy (..)
  , FromSomeType (..)
  )
where

import Cardano.Api.Hash
import Cardano.Api.Key.Internal
import Cardano.Api.Key.Internal.Class
import Cardano.Api.Key.Internal.Mnemonic
import Cardano.Api.Key.Internal.Praos
import Cardano.Api.Key.Internal.SomeAddressVerificationKey
