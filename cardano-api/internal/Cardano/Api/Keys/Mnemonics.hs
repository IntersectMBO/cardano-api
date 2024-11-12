{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Api.Keys.Mnemonics
  ( MnemonicSize (..)
  , generateMnemonic
  , MnemonicToSigningStakeKeyError (..)
  , SecondFactor
  , signingKeyFromMnemonic
  )
where

import           Cardano.Api.Error (Error (..))
import           Cardano.Api.Keys.Class (Key (..))
import           Cardano.Api.Keys.Shelley (AsType, PaymentExtendedKey,
                   SigningKey (PaymentExtendedSigningKey, StakeExtendedSigningKey),
                   StakeExtendedKey)

import           Cardano.Address.Derivation (Depth (..), DerivationType (..), HardDerivation (..),
                   Index, XPrv, genMasterKeyFromMnemonic, indexFromWord32)
import           Cardano.Address.Style.Shelley (Role (..), Shelley (..))
import           Cardano.Mnemonic (MkSomeMnemonic (mkSomeMnemonic), MkSomeMnemonicError (..),
                   SomeMnemonic, entropyToMnemonic, genEntropy, mnemonicToText, someMnemonicToBytes)

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteArray as BA
import           Data.ByteString (ByteString)
import           Data.Either.Combinators (mapLeft, maybeToRight)
import           Data.Either.Extra (maybeToEither)
import           Data.Text (Text)
import           Data.Word (Word32)
import           Prettyprinter (Doc, Pretty (..))

-- | The size of a mnemonic sentence.
-- The size is given in the number of words in the sentence.
-- The allowed sizes are 12, 15, 18, 21, and 24.
data MnemonicSize
  = MS_12
  | MS_15
  | MS_18
  | MS_21
  | MS_24
  deriving (Eq, Show)

-- | Generate a mnemonic sentence of the given size.
generateMnemonic
  :: MonadIO m
  => MnemonicSize
  -- ^ The size of the mnemonic sentence to generate.
  -- Must be one of 12, 15, 18, 21, or 24.
  -> m [Text]
generateMnemonic MS_12 = liftIO (mnemonicToText @12 . entropyToMnemonic <$> genEntropy)
generateMnemonic MS_15 = liftIO (mnemonicToText @15 . entropyToMnemonic <$> genEntropy)
generateMnemonic MS_18 = liftIO (mnemonicToText @18 . entropyToMnemonic <$> genEntropy)
generateMnemonic MS_21 = liftIO (mnemonicToText @21 . entropyToMnemonic <$> genEntropy)
generateMnemonic MS_24 = liftIO (mnemonicToText @24 . entropyToMnemonic <$> genEntropy)

-- | Errors that can occur when converting a mnemonic sentence to a signing key
-- using the 'signingStakeKeyFromMnemonic' function.
data MnemonicToSigningStakeKeyError
  = InvalidMnemonicError String
  | InvalidSecondFactorMnemonicError String
  | InvalidAccountNumberError Word32
  | InvalidPaymentKeyNoError Word32
  deriving (Eq, Show)

instance Error MnemonicToSigningStakeKeyError where
  prettyError :: MnemonicToSigningStakeKeyError -> Doc ann
  prettyError (InvalidMnemonicError str) = "Invalid mnemonic sentence: " <> pretty str
  prettyError (InvalidSecondFactorMnemonicError str) = "Invalid second factor mnemonic sentence: " <> pretty str
  prettyError (InvalidAccountNumberError accNo) = "Invalid account number: " <> pretty accNo
  prettyError (InvalidPaymentKeyNoError keyNo) = "Invalid payment key number: " <> pretty keyNo

-- | The second factor for the key derivation.
data SecondFactor
  = -- | Use a mnemonic sentence as the second factor.
    FromMnemonic [Text]
  | -- | Use a raw byte string as the second factor.
    FromByteString ByteString
  deriving (Eq, Show)

class ExtendedSigningKeyRole keyrole indexType where
  -- | Derive an extended private key of the keyrole from an account extended private key
  deriveSigningKeyFromAccount
    :: AsType keyrole -> Shelley 'AccountK XPrv -> indexType -> Either Word32 (SigningKey keyrole)

instance ExtendedSigningKeyRole PaymentExtendedKey Word32 where
  deriveSigningKeyFromAccount
    :: AsType PaymentExtendedKey
    -> Shelley 'AccountK XPrv
    -> Word32
    -> Either Word32 (SigningKey PaymentExtendedKey)
  deriveSigningKeyFromAccount _ accK idx = do
    payKeyIx <- maybeToEither idx $ indexFromWord32 @(Index 'Soft 'PaymentK) idx
    return $ PaymentExtendedSigningKey $ getKey $ deriveAddressPrivateKey accK UTxOExternal payKeyIx

instance ExtendedSigningKeyRole StakeExtendedKey Word32 where
  deriveSigningKeyFromAccount
    :: AsType StakeExtendedKey
    -> Shelley 'AccountK XPrv
    -> Word32
    -> Either Word32 (SigningKey StakeExtendedKey)
  deriveSigningKeyFromAccount _ accK idx = do
    payKeyIx <- maybeToEither idx $ indexFromWord32 @(Index 'Soft 'PaymentK) idx
    return $ StakeExtendedSigningKey $ getKey $ deriveAddressPrivateKey accK Stake payKeyIx

-- | Generate a signing key from a mnemonic sentence.
-- A derivation path is like a file path in a file system. It specifies the
-- location of a key in the key tree. The path is a list of indices, one for each
-- level of the tree. The indices are separated by a forward slash (/).
-- In this function we only ask for two indices: the account number and the
-- payment key number. Each account can have multiple payment keys.
signingKeyFromMnemonic
  :: ExtendedSigningKeyRole keyrole indexType
  => AsType keyrole
  -- ^ Type of the extended signing key to generate.
  -> [Text]
  -- ^ The mnemonic sentence. The length must be one of 12, 15, 18, 21, or 24.
  -- Each element of the list must be a single word.
  -> Maybe SecondFactor
  -- ^ The second factor for the key derivation. If 'Nothing', the key is derived
  -- without a second factor.
  -> Word32
  -- ^ The account number in the derivation path. First account is 0.
  -> indexType
  -- ^ The payment key number in the derivation path (as 'Word32') if applicable for
  -- the given key role, otherwise '()'. First key is 0.
  -> Either MnemonicToSigningStakeKeyError (SigningKey keyrole)
signingKeyFromMnemonic role mnemonicWords mSecondFactor accNo payKeyNo = do
  -- Convert raw types to the ones used in the cardano-addresses library
  someMnemonic <- mapLeft InvalidMnemonicError $ wordsToSomeMnemonic mnemonicWords
  secondFactorBytes <- toSecondFactor mSecondFactor
  accIx <-
    maybeToRight (InvalidAccountNumberError accNo) $
      indexFromWord32 @(Index 'Hardened 'AccountK) (0x80000000 + accNo)

  -- Derive the rootk key
  let rootK = genMasterKeyFromMnemonic someMnemonic secondFactorBytes :: Shelley 'RootK XPrv
      -- Derive the account key
      accK = deriveAccountPrivateKey rootK accIx

  -- Derive the extended private key
  mapLeft InvalidPaymentKeyNoError $ deriveSigningKeyFromAccount role accK payKeyNo
 where
  -- Convert the ByteString to a SigningKey

  -- Convert the mnemonic sentence to a SomeMnemonic value
  wordsToSomeMnemonic :: [Text] -> Either String SomeMnemonic
  wordsToSomeMnemonic = mapLeft getMkSomeMnemonicError . mkSomeMnemonic @[12, 15, 18, 21, 24]

  -- Convert the second factor to a ScrubbedBytes value or mempty if none
  toSecondFactor :: Maybe SecondFactor -> Either MnemonicToSigningStakeKeyError BA.ScrubbedBytes
  toSecondFactor Nothing = return mempty
  toSecondFactor (Just (FromMnemonic secondFactorWords)) =
    someMnemonicToBytes
      <$> mapLeft InvalidSecondFactorMnemonicError (wordsToSomeMnemonic secondFactorWords)
  toSecondFactor (Just (FromByteString secondFactorBytes)) =
    return $ BA.convert secondFactorBytes
