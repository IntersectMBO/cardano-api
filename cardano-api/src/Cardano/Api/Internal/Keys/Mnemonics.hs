{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.Internal.Keys.Mnemonics
  ( MnemonicSize (..)
  , generateMnemonic
  , MnemonicToSigningKeyError (..)
  , signingKeyFromMnemonic
  , signingKeyFromMnemonicWithPaymentKeyIndex
  , findMnemonicWordsWithPrefix
  , autocompleteMnemonicPrefix
  )
where

import Cardano.Api.Internal.Error (Error (..))
import Cardano.Api.Internal.Keys.Class (Key (..))
import Cardano.Api.Internal.Keys.Shelley
  ( AsType
  , CommitteeColdExtendedKey
  , CommitteeHotExtendedKey
  , DRepExtendedKey
  , PaymentExtendedKey
  , SigningKey (..)
  , StakeExtendedKey
  )

import Cardano.Address.Derivation
  ( Depth (..)
  , DerivationType (..)
  , HardDerivation (..)
  , Index
  , XPrv
  , genMasterKeyFromMnemonic
  , indexFromWord32
  )
import Cardano.Address.Style.Shelley
  ( Role (..)
  , Shelley (..)
  , deriveCCColdPrivateKey
  , deriveCCHotPrivateKey
  , deriveDRepPrivateKey
  )
import Cardano.Crypto.Encoding.BIP39 (Dictionary (dictionaryIndexToWord))
import Cardano.Mnemonic
  ( MkSomeMnemonic (mkSomeMnemonic)
  , MkSomeMnemonicError (..)
  , SomeMnemonic
  , entropyToMnemonic
  , genEntropy
  , mnemonicToText
  )

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.Either.Combinators (mapLeft, maybeToRight)
import Data.Either.Extra (maybeToEither)
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Trie (submap)
import Data.Trie qualified as Trie
import Data.Trie.Convenience qualified as Trie
import Data.Word (Word32)
import Prettyprinter (Doc, Pretty (..))

import Basement.Compat.IsList qualified as Basement
import Basement.String qualified as Basement
import Crypto.Encoding.BIP39.English (english)

-- | The size of a mnemonic sentence.
-- The size is given in the number of words in the sentence.
-- The allowed sizes are 12, 15, 18, 21, and 24.
data MnemonicSize
  = MS12
  | MS15
  | MS18
  | MS21
  | MS24
  deriving (Eq, Show)

-- | Generate a mnemonic sentence of the given size.
generateMnemonic
  :: MonadIO m
  => MnemonicSize
  -- ^ The size of the mnemonic sentence to generate.
  -- Must be one of 12, 15, 18, 21, or 24.
  -> m [Text]
generateMnemonic MS12 = liftIO (mnemonicToText @12 . entropyToMnemonic <$> genEntropy)
generateMnemonic MS15 = liftIO (mnemonicToText @15 . entropyToMnemonic <$> genEntropy)
generateMnemonic MS18 = liftIO (mnemonicToText @18 . entropyToMnemonic <$> genEntropy)
generateMnemonic MS21 = liftIO (mnemonicToText @21 . entropyToMnemonic <$> genEntropy)
generateMnemonic MS24 = liftIO (mnemonicToText @24 . entropyToMnemonic <$> genEntropy)

-- | Errors that can occur when converting a mnemonic sentence to a signing key
data MnemonicToSigningKeyError
  = InvalidMnemonicError String
  | InvalidAccountNumberError Word32
  | InvalidPaymentKeyNoError Word32
  deriving (Eq, Show)

-- For information about address derivation check:
--  * https://cips.cardano.org/cip/CIP-1852
--  * https://github.com/uniVocity/cardano-tutorials/blob/master/cardano-addresses.md#understanding-the-hd-wallet-address-format-bip-44
--  * https://cips.cardano.org/cip/CIP-0105
instance Error MnemonicToSigningKeyError where
  prettyError :: MnemonicToSigningKeyError -> Doc ann
  prettyError (InvalidMnemonicError str) = "Invalid mnemonic sentence: " <> pretty str
  prettyError (InvalidAccountNumberError accNo) = "Invalid account number: " <> pretty accNo
  prettyError (InvalidPaymentKeyNoError keyNo) = "Invalid payment key number: " <> pretty keyNo

-- | Key roles that can be derived from a mnemonic sentence and only accept
-- one key per account number.
--
-- We derive one key per account following the advice in https://cips.cardano.org/cip/CIP-0105:
-- "Since it is best practice to use a single cryptographic key for a single purpose,
-- we opt to keep DRep and committee keys separate from other keys in Cardano."
--
-- We still need to specify a payment key number for payment and stake keys,
-- see 'IndexedSigningKeyFromRootKey' class for those roles (payment and stake keys).
class SigningKeyFromRootKey keyrole where
  -- | Derive an extended private key of the keyrole from an account extended private key
  deriveSigningKeyFromAccount
    :: AsType keyrole
    -- ^ Type of the extended signing key to generate.
    -> Shelley 'AccountK XPrv
    -- ^ The account extended private key from which to derivate the private key for the keyrole.
    -> SigningKey keyrole
    -- ^ The derived extended signing key or the 'indexType' if it is 'Word32' and it is invalid.

-- | Key roles that can be derived from a mnemonic sentence and accept multiple keys
-- per account number. For other key roles (DRep, and committee keys), see 'SigningKeyFromRootKey'.
class IndexedSigningKeyFromRootKey keyrole where
  -- | Derive an extended private key of the keyrole from an account extended private key
  deriveSigningKeyFromAccountWithPaymentKeyIndex
    :: AsType keyrole
    -- ^ Type of the extended signing key to generate.
    -> Shelley 'AccountK XPrv
    -- ^ The account extended private key from which to derivate the private key for the keyrole.
    -> Word32
    -- ^ The payment key number in the derivation path.
    -> Either Word32 (SigningKey keyrole)
    -- ^ The derived extended signing key or the 'indexType' if it is invalid.

instance IndexedSigningKeyFromRootKey PaymentExtendedKey where
  deriveSigningKeyFromAccountWithPaymentKeyIndex
    :: AsType PaymentExtendedKey
    -> Shelley 'AccountK XPrv
    -> Word32
    -> Either Word32 (SigningKey PaymentExtendedKey)
  deriveSigningKeyFromAccountWithPaymentKeyIndex _ accK idx = do
    payKeyIx <- maybeToEither idx $ indexFromWord32 @(Index 'Soft 'PaymentK) idx
    return $ PaymentExtendedSigningKey $ getKey $ deriveAddressPrivateKey accK UTxOExternal payKeyIx

instance IndexedSigningKeyFromRootKey StakeExtendedKey where
  deriveSigningKeyFromAccountWithPaymentKeyIndex
    :: AsType StakeExtendedKey
    -> Shelley 'AccountK XPrv
    -> Word32
    -> Either Word32 (SigningKey StakeExtendedKey)
  deriveSigningKeyFromAccountWithPaymentKeyIndex _ accK idx = do
    payKeyIx <- maybeToEither idx $ indexFromWord32 @(Index 'Soft 'PaymentK) idx
    return $ StakeExtendedSigningKey $ getKey $ deriveAddressPrivateKey accK Stake payKeyIx

instance SigningKeyFromRootKey DRepExtendedKey where
  deriveSigningKeyFromAccount
    :: AsType DRepExtendedKey
    -> Shelley 'AccountK XPrv
    -> SigningKey DRepExtendedKey
  deriveSigningKeyFromAccount _ accK =
    DRepExtendedSigningKey $ getKey $ deriveDRepPrivateKey accK

instance SigningKeyFromRootKey CommitteeColdExtendedKey where
  deriveSigningKeyFromAccount
    :: AsType CommitteeColdExtendedKey
    -> Shelley 'AccountK XPrv
    -> SigningKey CommitteeColdExtendedKey
  deriveSigningKeyFromAccount _ accK =
    CommitteeColdExtendedSigningKey $ getKey $ deriveCCColdPrivateKey accK

instance SigningKeyFromRootKey CommitteeHotExtendedKey where
  deriveSigningKeyFromAccount
    :: AsType CommitteeHotExtendedKey
    -> Shelley 'AccountK XPrv
    -> SigningKey CommitteeHotExtendedKey
  deriveSigningKeyFromAccount _ accK =
    CommitteeHotExtendedSigningKey $ getKey $ deriveCCHotPrivateKey accK

-- | Generate a signing key from a mnemonic sentence given a function that
-- derives a key from an account extended key.
signingKeyFromMnemonicWithDerivationFunction
  :: (Shelley AccountK XPrv -> Either Word32 (SigningKey keyrole))
  -- ^ Function to derive the signing key from the account key.
  -> [Text]
  -- ^ The mnemonic sentence. The length must be one of 12, 15, 18, 21, or 24.
  -- Each element of the list must be a single word.
  -> Word32
  -- ^ The account number in the derivation path. First account is 0.
  -> Either MnemonicToSigningKeyError (SigningKey keyrole)
signingKeyFromMnemonicWithDerivationFunction derivationFunction mnemonicWords accNo = do
  -- Convert raw types to the ones used in the cardano-addresses library
  someMnemonic <- mapLeft InvalidMnemonicError $ wordsToSomeMnemonic mnemonicWords
  accIx <-
    maybeToRight (InvalidAccountNumberError accNo) $
      indexFromWord32 @(Index 'Hardened 'AccountK) (0x80000000 + accNo)

  -- Derive the rootk key
  let rootK = genMasterKeyFromMnemonic someMnemonic mempty :: Shelley 'RootK XPrv
      -- Derive the account key
      accK = deriveAccountPrivateKey rootK accIx

  -- Derive the extended private key
  mapLeft InvalidPaymentKeyNoError $ derivationFunction accK
 where
  wordsToSomeMnemonic :: [Text] -> Either String SomeMnemonic
  wordsToSomeMnemonic = mapLeft getMkSomeMnemonicError . mkSomeMnemonic @[12, 15, 18, 21, 24]

-- | Generate a signing key from a mnemonic sentence for a key role that
-- accepts several payment keys from an account number (extended payment and stake keys).
-- For other key roles (DRep and committee keys), see 'signingKeyFromMnemonic'.
--
-- A derivation path is like a file path in a file system. It specifies the
-- location of a key in the key tree. The path is a list of indices, one for each
-- level of the tree. The indices are separated by a forward slash (/).
-- In this function, we only ask for two indices: the account number and the
-- payment key number. Each account can have multiple payment keys.
--
-- For more information about address derivation, check:
--  * https://cips.cardano.org/cip/CIP-1852
--  * https://github.com/uniVocity/cardano-tutorials/blob/master/cardano-addresses.md#understanding-the-hd-wallet-address-format-bip-44
--  * https://cips.cardano.org/cip/CIP-0105
signingKeyFromMnemonicWithPaymentKeyIndex
  :: IndexedSigningKeyFromRootKey keyrole
  => AsType keyrole
  -- ^ Type of the extended signing key to generate.
  -> [Text]
  -- ^ The mnemonic sentence. The length must be one of 12, 15, 18, 21, or 24.
  -- Each element of the list must be a single word.
  -> Word32
  -- ^ The account number in the derivation path. The first account is 0.
  -> Word32
  -- ^ The payment key number in the derivation path.
  --
  -- Consider that wallets following the BIP-44 standard only check 20 addresses
  -- without transactions before giving up. For example, if you have a fresh wallet
  -- and receive a payment on the address generated with address_index = 6, your
  -- wallet may only display the money received on addresses from 0 to 26.
  -- If you receive payment on an address with address_index = 30, the funds may not
  -- be displayed to you even though it's on the blockchain. It will only appear
  -- once there is a transaction in some address where address_index is between 10
  -- and 29. The gap limit can be customized on some wallets, but increasing it
  -- reduces synchronization performance.
  -> Either MnemonicToSigningKeyError (SigningKey keyrole)
signingKeyFromMnemonicWithPaymentKeyIndex keyRole mnemonicWords accNo payKeyNo = do
  signingKeyFromMnemonicWithDerivationFunction
    (\accK -> deriveSigningKeyFromAccountWithPaymentKeyIndex keyRole accK payKeyNo)
    mnemonicWords
    accNo

-- | Generate a signing key from a mnemonic sentence for a key role that
-- accepts only one payment key from an account number (DRep and committee keys).
-- For other key roles (extended payment and stake keys), see 'signingKeyFromMnemonicWithPaymentKeyIndex'.
--
-- We derive one key per account following the advice in https://cips.cardano.org/cip/CIP-0105:
-- "Since it is best practice to use a single cryptographic key for a single purpose,
-- we opt to keep DRep and committee keys separate from other keys in Cardano."
--
-- A derivation path is like a file path in a file system. It specifies the
-- location of a key in the key tree. The path is a list of indices, one for each
-- level of the tree. The indices are separated by a forward slash (/).
-- In this function we only ask for one index: the account number.
--
-- For more information about address derivation check:
--  * https://cips.cardano.org/cip/CIP-1852
--  * https://github.com/uniVocity/cardano-tutorials/blob/master/cardano-addresses.md#understanding-the-hd-wallet-address-format-bip-44
--  * https://cips.cardano.org/cip/CIP-0105
signingKeyFromMnemonic
  :: SigningKeyFromRootKey keyrole
  => AsType keyrole
  -- ^ Type of the extended signing key to generate.
  -> [Text]
  -- ^ The mnemonic sentence. The length must be one of 12, 15, 18, 21, or 24.
  -- Each element of the list must be a single word.
  -> Word32
  -- ^ The account number in the derivation path. First account is 0.
  -> Either MnemonicToSigningKeyError (SigningKey keyrole)
signingKeyFromMnemonic keyRole mnemonicWords accNo = do
  signingKeyFromMnemonicWithDerivationFunction
    (return . deriveSigningKeyFromAccount keyRole)
    mnemonicWords
    accNo

-- | Obtain the list of all mnemonic words that start with the given prefix and their index in the dictionary.
-- For example:
-- >>> findMnemonicWordsWithPrefix "cha"
-- [("chair",302),("chalk",303),("champion",304),("change",305),("chaos",306),("chapter",307),("charge",308),("chase",309),("chat",310)]
findMnemonicWordsWithPrefix :: Text -> [(Text, Int)]
findMnemonicWordsWithPrefix word = toList $ map (first decodeUtf8) $ Trie.toList matchingSubTrie
 where
  matchingSubTrie :: Trie.Trie Int
  matchingSubTrie = submap (encodeUtf8 word) englishMnemonicTrie

-- | Autocomplete the prefix of the mnemonic word as much as possible.
-- In other words, find the longest common prefix for all the words
-- that start with the given prefix.
-- For example:
-- >>> autocompleteMnemonicPrefix "ty"
-- Just "typ"
--
-- Because "type" and "typical" are the only words that start with "ty".
--
-- >>> autocompleteMnemonicPrefix "vani"
-- Just "vanish"
--
-- Because "vanish" is the only word that starts with "vani".
--
-- >>> autocompleteMnemonicPrefix "medo"
-- Nothing
--
-- Because there are no words that start with "medo".
autocompleteMnemonicPrefix :: Text -> Maybe Text
autocompleteMnemonicPrefix word =
  let subtrie = matchingSubTrie word englishMnemonicTrie
      matches = toList $ map (first decodeUtf8) $ Trie.toList subtrie
      numMatches = Trie.size subtrie
   in case matches of
        [] -> Nothing
        (firstMatch, _) : _ -> expandWhileSameNumberOfMatches numMatches word (Text.drop (Text.length word) firstMatch) subtrie
 where
  matchingSubTrie :: Text -> Trie.Trie Int -> Trie.Trie Int
  matchingSubTrie w = submap (encodeUtf8 w)

  expandWhileSameNumberOfMatches :: Int -> Text -> Text -> Trie.Trie Int -> Maybe Text
  expandWhileSameNumberOfMatches numMatches curPrefix potentialExtensions subTrie =
    case Text.uncons potentialExtensions of
      Nothing -> Just curPrefix
      Just (newChar, remainingPotentialExtensions) ->
        let potentialNewPrefix = Text.snoc curPrefix newChar
            newSubTrie = matchingSubTrie potentialNewPrefix subTrie
         in if Trie.size newSubTrie == numMatches
              then
                expandWhileSameNumberOfMatches numMatches potentialNewPrefix remainingPotentialExtensions newSubTrie
              else Just curPrefix

-- | Trie of English mnemonic words with their index.
englishMnemonicTrie :: Trie.Trie Int
englishMnemonicTrie =
  Trie.fromListL
    ( map
        ( \i ->
            (,fromEnum i) $
              BS.pack . Basement.toList . Basement.toBytes Basement.UTF8 $
                dictionaryIndexToWord english i
        )
        [minBound .. maxBound]
    )
