{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Api.LedgerState.Fold.Core
  ( FoldStatus(..)
  , LedgerStateCondition(..)
  , FoldBlocksError(..)

  , handleIOExceptions
  ) where

import           Cardano.Api.Error as Api
import           Cardano.Api.LedgerState.Core
import           Cardano.Api.Monad.Error
import           Cardano.Api.Pretty

import           Control.Exception.Safe
import           Control.Monad
import           Data.Bifunctor

data FoldBlocksError
  = FoldBlocksInitialLedgerStateError !InitialLedgerStateError
  | FoldBlocksApplyBlockError !LedgerStateError
  | FoldBlocksIOException !IOException
  deriving Show

instance Error FoldBlocksError where
  prettyError = \case
    FoldBlocksInitialLedgerStateError err -> prettyError err
    FoldBlocksApplyBlockError err -> "Failed when applying a block:" <+> prettyError err
    FoldBlocksIOException err -> "IOException:" <+> prettyException err

-- | Type that lets us decide whether to continue or stop
-- the fold from within our accumulation function.
data FoldStatus
  = ContinueFold
  | StopFold
  | DebugFold
  deriving (Show, Eq)

data LedgerStateCondition
  = ConditionMet
  | ConditionNotMet
  deriving (Show, Eq)

handleIOExceptions :: MonadIOTransError FoldBlocksError t m => ExceptT FoldBlocksError IO a -> t m a
handleIOExceptions = liftEither <=< liftIO . fmap (join . first FoldBlocksIOException) . try . runExceptT
