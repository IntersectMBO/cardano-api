-- |
-- >>> findMnemonicWordsWithPrefix "cha"
-- [("chair",302),("chalk",303),("champion",304),("change",305),("chaos",306),("chapter",307),("charge",308),("chase",309),("chat",310)]
--
-- >>> autocompleteMnemonicPrefix "ty"
-- Just "typ"
--
-- >>> autocompleteMnemonicPrefix "vani"
-- Just "vanish"
--
-- >>> autocompleteMnemonicPrefix "medo"
-- Nothing
module Cardano.Api.Key
  ( -- * The Key class
    Key (..)
  , generateSigningKey
  , generateInsecureSigningKey
  , CastVerificationKeyRole (..)
  , CastSigningKeyRole (..)
  , AsType (AsVerificationKey, AsSigningKey, AsBlsKey)

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

    -- * Leios

    -- | BLS12-381 key type
  , BlsKey
  , BlsPossessionProof
  , blsPossessionProof
  , createBlsPossessionProof

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
import Cardano.Api.Key.Internal.Leios
import Cardano.Api.Key.Internal.Mnemonic
import Cardano.Api.Key.Internal.Praos
import Cardano.Api.Key.Internal.SomeAddressVerificationKey
