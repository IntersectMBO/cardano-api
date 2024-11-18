{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.Keys.Mnemonics
  ( MnemonicSize (..)
  , generateMnemonic
  , MnemonicToSigningKeyError (..)
  , signingKeyFromMnemonic
  , findMnemonicWordsWithPrefix
  , autocompleteMnemonicPrefix
  )
where

import           Cardano.Api.Error (Error (..))
import           Cardano.Api.Keys.Class (Key (..))
import           Cardano.Api.Keys.Shelley (AsType, CommitteeColdExtendedKey,
                   CommitteeHotExtendedKey, DRepExtendedKey, PaymentExtendedKey, SigningKey (..),
                   StakeExtendedKey)

import           Cardano.Address.Derivation (Depth (..), DerivationType (..), HardDerivation (..),
                   Index, XPrv, genMasterKeyFromMnemonic, indexFromWord32)
import           Cardano.Address.Style.Shelley (Role (..), Shelley (..), deriveCCColdPrivateKey,
                   deriveCCHotPrivateKey, deriveDRepPrivateKey)
import           Cardano.Crypto.Encoding.BIP39 (Dictionary (dictionaryIndexToWord))
import           Cardano.Mnemonic (MkSomeMnemonic (mkSomeMnemonic), MkSomeMnemonicError (..),
                   SomeMnemonic, entropyToMnemonic, genEntropy, mnemonicToText)

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Bifunctor (first)
import qualified Data.ByteString as BS
import           Data.Either.Combinators (mapLeft, maybeToRight)
import           Data.Either.Extra (maybeToEither)
import           Data.Foldable (toList)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.Trie (submap)
import qualified Data.Trie as Trie
import qualified Data.Trie.Convenience as Trie
import           Data.Word (Word32)
import           Prettyprinter (Doc, Pretty (..))

import qualified Basement.Compat.IsList as Basement
import qualified Basement.String as Basement
import           Crypto.Encoding.BIP39.English (english)

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
-- using the 'signingStakeKeyFromMnemonic' function.
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

class ExtendedSigningKeyRole keyrole where
  -- | The type for the payment key number in the derivation path (i.e: 'Word32' if applicable or
  -- '()' if not). See 'deriveSigningKeyFromAccount' for more information.
  type EskrPaymentAddrIndex keyrole

  -- | Derive an extended private key of the keyrole from an account extended private key
  deriveSigningKeyFromAccount
    :: AsType keyrole
    -- ^ Type of the extended signing key to generate.
    -> Shelley 'AccountK XPrv
    -- ^ The account extended private key from which to derivate the private key for the keyrole.
    -> EskrPaymentAddrIndex keyrole
    -- ^ The payment key number in the derivation path (as 'Word32') if applicable for
    -- the given key role, otherwise '()'. First key is 0.
    --
    -- As specified by https://cips.cardano.org/cip/CIP-0105:
    -- Since it is best practice to use a single cryptographic key for a single purpose,
    -- we opt to keep DRep and committee keys separate from other keys in Cardano.
    -- But we still need to specify a payment key number for payment and stake keys.
    -> Either Word32 (SigningKey keyrole)
    -- ^ The derived extended signing key or the 'indexType' if it is 'Word32' and it is invalid.

instance ExtendedSigningKeyRole PaymentExtendedKey where
  type EskrPaymentAddrIndex PaymentExtendedKey = Word32
  deriveSigningKeyFromAccount
    :: AsType PaymentExtendedKey
    -> Shelley 'AccountK XPrv
    -> Word32
    -> Either Word32 (SigningKey PaymentExtendedKey)
  deriveSigningKeyFromAccount _ accK idx = do
    payKeyIx <- maybeToEither idx $ indexFromWord32 @(Index 'Soft 'PaymentK) idx
    return $ PaymentExtendedSigningKey $ getKey $ deriveAddressPrivateKey accK UTxOExternal payKeyIx

instance ExtendedSigningKeyRole StakeExtendedKey where
  type EskrPaymentAddrIndex StakeExtendedKey = Word32
  deriveSigningKeyFromAccount
    :: AsType StakeExtendedKey
    -> Shelley 'AccountK XPrv
    -> Word32
    -> Either Word32 (SigningKey StakeExtendedKey)
  deriveSigningKeyFromAccount _ accK idx = do
    payKeyIx <- maybeToEither idx $ indexFromWord32 @(Index 'Soft 'PaymentK) idx
    return $ StakeExtendedSigningKey $ getKey $ deriveAddressPrivateKey accK Stake payKeyIx

instance ExtendedSigningKeyRole DRepExtendedKey where
  type EskrPaymentAddrIndex DRepExtendedKey = ()
  deriveSigningKeyFromAccount
    :: AsType DRepExtendedKey
    -> Shelley 'AccountK XPrv
    -> ()
    -- As specified by https://cips.cardano.org/cip/CIP-0105:
    -- Since it is best practice to use a single cryptographic key for a single purpose,
    -- we opt to keep DRep and committee keys separate from other keys in Cardano.
    -- Therefore, we do not need to specify a payment key number for DRep keys.
    -> Either Word32 (SigningKey DRepExtendedKey)
  deriveSigningKeyFromAccount _ accK _ =
    return $ DRepExtendedSigningKey $ getKey $ deriveDRepPrivateKey accK

instance ExtendedSigningKeyRole CommitteeColdExtendedKey where
  type EskrPaymentAddrIndex CommitteeColdExtendedKey = ()
  deriveSigningKeyFromAccount
    :: AsType CommitteeColdExtendedKey
    -> Shelley 'AccountK XPrv
    -> ()
    -- As specified by https://cips.cardano.org/cip/CIP-0105:
    -- Since it is best practice to use a single cryptographic key for a single purpose,
    -- we opt to keep DRep and committee keys separate from other keys in Cardano.
    -- Therefore, we do not need to specify a payment key number for cold committee keys.
    -> Either Word32 (SigningKey CommitteeColdExtendedKey)
  deriveSigningKeyFromAccount _ accK _ =
    return $ CommitteeColdExtendedSigningKey $ getKey $ deriveCCColdPrivateKey accK

instance ExtendedSigningKeyRole CommitteeHotExtendedKey where
  type EskrPaymentAddrIndex CommitteeHotExtendedKey = ()
  deriveSigningKeyFromAccount
    :: AsType CommitteeHotExtendedKey
    -> Shelley 'AccountK XPrv
    -> ()
    -- As specified by https://cips.cardano.org/cip/CIP-0105:
    -- Since it is best practice to use a single cryptographic key for a single purpose,
    -- we opt to keep DRep and committee keys separate from other keys in Cardano.
    -- Therefore, we do not need to specify a payment key number for hot committee keys.
    -> Either Word32 (SigningKey CommitteeHotExtendedKey)
  deriveSigningKeyFromAccount _ accK _ =
    return $ CommitteeHotExtendedSigningKey $ getKey $ deriveCCHotPrivateKey accK

-- | Generate a signing key from a mnemonic sentence.
-- A derivation path is like a file path in a file system. It specifies the
-- location of a key in the key tree. The path is a list of indices, one for each
-- level of the tree. The indices are separated by a forward slash (/).
-- In this function we only ask for two indices: the account number and the
-- payment key number. Each account can have multiple payment keys.
--
-- For more information about address derivation check:
--  * https://cips.cardano.org/cip/CIP-1852
--  * https://github.com/uniVocity/cardano-tutorials/blob/master/cardano-addresses.md#understanding-the-hd-wallet-address-format-bip-44
--  * https://cips.cardano.org/cip/CIP-0105
signingKeyFromMnemonic
  :: ExtendedSigningKeyRole keyrole
  => AsType keyrole
  -- ^ Type of the extended signing key to generate.
  -> [Text]
  -- ^ The mnemonic sentence. The length must be one of 12, 15, 18, 21, or 24.
  -- Each element of the list must be a single word.
  -> Word32
  -- ^ The account number in the derivation path. First account is 0.
  -> EskrPaymentAddrIndex keyrole
  -- ^ The payment key number in the derivation path (as 'Word32') if applicable for
  -- the given key role, otherwise '()'. First key is 0.
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
signingKeyFromMnemonic role mnemonicWords accNo payKeyNo = do
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
  mapLeft InvalidPaymentKeyNoError $ deriveSigningKeyFromAccount role accK payKeyNo
 where
  -- Convert the ByteString to a SigningKey

  -- Convert the mnemonic sentence to a SomeMnemonic value
  wordsToSomeMnemonic :: [Text] -> Either String SomeMnemonic
  wordsToSomeMnemonic = mapLeft getMkSomeMnemonicError . mkSomeMnemonic @[12, 15, 18, 21, 24]

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
