{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.Protocol
  ( BlockType(..)
  , SomeBlockType (..)
  , reflBlockType
  , Protocol(..)
  , ProtocolInfoArgs(..)
  , ProtocolClient(..)
  , ProtocolClientInfoArgs(..)
  ) where

import           Cardano.Api.Modes

import           Ouroboros.Consensus.Block.Forging (BlockForging)
import qualified Ouroboros.Consensus.Byron.Ledger as Byron
import           Ouroboros.Consensus.Cardano
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.ByronHFC (ByronBlockHFC)
import           Ouroboros.Consensus.Cardano.Node
import           Ouroboros.Consensus.HardFork.Combinator.Embed.Unary
import qualified Ouroboros.Consensus.Ledger.SupportsProtocol as Consensus
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolClientInfo (..), ProtocolInfo (..))
import           Ouroboros.Consensus.Node.Run (RunNode)
import qualified Ouroboros.Consensus.Protocol.TPraos as Consensus
import qualified Ouroboros.Consensus.Shelley.Eras as Consensus
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as Consensus
import           Ouroboros.Consensus.Shelley.ShelleyHFC (ShelleyBlockHFC)
import           Ouroboros.Consensus.Util.IOLike (IOLike)

import           Data.Bifunctor (bimap)

import           Type.Reflection ((:~:) (..))

class (RunNode blk, IOLike m) => Protocol m blk where
  data ProtocolInfoArgs blk
  protocolInfo :: ProtocolInfoArgs blk -> (ProtocolInfo blk, m [BlockForging m blk])

-- | Node client support for each consensus protocol.
--
-- This is like 'Protocol' but for clients of the node, so with less onerous
-- requirements than to run a node.
--
class RunNode blk => ProtocolClient blk where
  data ProtocolClientInfoArgs blk
  protocolClientInfo :: ProtocolClientInfoArgs blk -> ProtocolClientInfo blk


-- | Run PBFT against the Byron ledger
instance IOLike m => Protocol m ByronBlockHFC where
  data ProtocolInfoArgs ByronBlockHFC = ProtocolInfoArgsByron (ProtocolParams Byron.ByronBlock)
  protocolInfo (ProtocolInfoArgsByron params) = ( inject $ protocolInfoByron params
                                                , pure . map inject $ blockForgingByron params
                                                )

instance (CardanoHardForkConstraints StandardCrypto, IOLike m) => Protocol m (CardanoBlock StandardCrypto) where
  data ProtocolInfoArgs (CardanoBlock StandardCrypto) =
         ProtocolInfoArgsCardano
          (CardanoProtocolParams StandardCrypto)

  protocolInfo (ProtocolInfoArgsCardano paramsCardano) =
    protocolInfoCardano paramsCardano

instance ProtocolClient ByronBlockHFC where
  data ProtocolClientInfoArgs ByronBlockHFC =
    ProtocolClientInfoArgsByron EpochSlots
  protocolClientInfo (ProtocolClientInfoArgsByron epochSlots) =
    inject $ protocolClientInfoByron epochSlots

instance CardanoHardForkConstraints StandardCrypto => ProtocolClient (CardanoBlock StandardCrypto) where
  data ProtocolClientInfoArgs (CardanoBlock StandardCrypto) =
    ProtocolClientInfoArgsCardano EpochSlots
  protocolClientInfo (ProtocolClientInfoArgsCardano epochSlots) =
    protocolClientInfoCardano epochSlots

instance ( IOLike m
         , Consensus.LedgerSupportsProtocol
             (Consensus.ShelleyBlock
                (Consensus.TPraos StandardCrypto) (ShelleyEra StandardCrypto))
         )
  => Protocol m (ShelleyBlockHFC (Consensus.TPraos StandardCrypto) StandardShelley) where
  data ProtocolInfoArgs (ShelleyBlockHFC (Consensus.TPraos StandardCrypto) StandardShelley) = ProtocolInfoArgsShelley
    (ShelleyGenesis StandardCrypto)
    (ProtocolParamsShelleyBased StandardCrypto)
    (ProtocolParams (Consensus.ShelleyBlock (Consensus.TPraos StandardCrypto) StandardShelley))
  protocolInfo (ProtocolInfoArgsShelley genesis paramsShelleyBased_ paramsShelley_) =
    bimap inject (fmap $ map inject) $ protocolInfoShelley genesis paramsShelleyBased_ paramsShelley_

instance Consensus.LedgerSupportsProtocol
          (Consensus.ShelleyBlock
            (Consensus.TPraos StandardCrypto) (Consensus.ShelleyEra StandardCrypto))
  => ProtocolClient (ShelleyBlockHFC (Consensus.TPraos StandardCrypto) StandardShelley) where
  data ProtocolClientInfoArgs (ShelleyBlockHFC (Consensus.TPraos StandardCrypto) StandardShelley) =
    ProtocolClientInfoArgsShelley
  protocolClientInfo ProtocolClientInfoArgsShelley =
    inject protocolClientInfoShelley

data BlockType blk where
  ByronBlockType :: BlockType ByronBlockHFC
  ShelleyBlockType :: BlockType (ShelleyBlockHFC (Consensus.TPraos StandardCrypto) StandardShelley)
  CardanoBlockType :: BlockType (CardanoBlock StandardCrypto)

deriving instance Eq (BlockType blk)
deriving instance Show (BlockType blk)

reflBlockType :: BlockType blk -> BlockType blk' -> Maybe (blk :~: blk')
reflBlockType ByronBlockType   ByronBlockType   = Just Refl
reflBlockType ShelleyBlockType ShelleyBlockType = Just Refl
reflBlockType CardanoBlockType CardanoBlockType = Just Refl
reflBlockType _                _                = Nothing


data SomeBlockType where
  SomeBlockType :: BlockType blk -> SomeBlockType

deriving instance Show SomeBlockType
