{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Api.Keys.Mnemonics
  ( MnemonicSize (..)
  , generateMnemonic
  , MnemonicToSigningStakeKeyError (..)
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
                   SomeMnemonic, entropyToMnemonic, genEntropy, mnemonicToText)

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Either.Combinators (mapLeft, maybeToRight)
import           Data.Either.Extra (maybeToEither)
import           Data.Text (Text)
import           Data.Word (Word32)
import           Prettyprinter (Doc, Pretty (..))

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
data MnemonicToSigningStakeKeyError
  = InvalidMnemonicError String
  | InvalidAccountNumberError Word32
  | InvalidPaymentKeyNoError Word32
  deriving (Eq, Show)

instance Error MnemonicToSigningStakeKeyError where
  prettyError :: MnemonicToSigningStakeKeyError -> Doc ann
  prettyError (InvalidMnemonicError str) = "Invalid mnemonic sentence: " <> pretty str
  prettyError (InvalidAccountNumberError accNo) = "Invalid account number: " <> pretty accNo
  prettyError (InvalidPaymentKeyNoError keyNo) = "Invalid payment key number: " <> pretty keyNo

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
  -> Word32
  -- ^ The account number in the derivation path. First account is 0.
  -> indexType
  -- ^ The payment key number in the derivation path (as 'Word32') if applicable for
  -- the given key role, otherwise '()'. First key is 0.
  -> Either MnemonicToSigningStakeKeyError (SigningKey keyrole)
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
