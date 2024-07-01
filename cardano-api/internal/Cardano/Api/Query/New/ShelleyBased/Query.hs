{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.Query.New.ShelleyBased.Query where

import           Cardano.Api.Certificate
import           Cardano.Api.Eon.ShelleyBasedEra
import           Cardano.Api.Eras
import           Cardano.Api.IPC
import           Cardano.Api.Keys.Shelley
import           Cardano.Api.Query
import           Cardano.Api.Query.New.EraIndependent.Expr

import qualified Cardano.Ledger.Credential as Shelley
import qualified Cardano.Ledger.Shelley.API as Shelley
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import qualified Ouroboros.Consensus.Block as Consensus
import qualified Ouroboros.Consensus.Byron.Ledger as Consensus
import qualified Ouroboros.Consensus.Cardano.Block as Consensus
import qualified Ouroboros.Consensus.Cardano.ByronHFC as Consensus
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import qualified Ouroboros.Consensus.Ledger.Query as Consensus
import qualified Ouroboros.Consensus.Protocol.Praos as Consensus
import qualified Ouroboros.Consensus.Protocol.TPraos as Consensus
import qualified Ouroboros.Consensus.Shelley.HFEras as Consensus
import qualified Ouroboros.Consensus.Shelley.Ledger.Query as Consensus
import qualified Ouroboros.Consensus.Shelley.ShelleyHFC as Consensus
import           Ouroboros.Network.Protocol.LocalStateQuery.Client (Some (..))

import           Data.Set (Set)
import qualified Data.Set as Set

data ShelleyBasedQueryError
  = ShelleyBasedQueryEraMismatch EraMismatch
  | ShelleyBasedEquerySimpleError IndependentEraQueryError
  deriving Show


data QueryShelleyBasedEra era result where
  QueryShelleyBasedEra
    :: QueryInShelleyBasedEra era result
    -> QueryShelleyBasedEra era (Either EraMismatch result)


createBlockQuery :: Consensus.BlockQuery blk result -> Consensus.Query blk result
createBlockQuery = Consensus.BlockQuery

-- TODO: This type checks however we need it to instantiate the correct "ShelleyBlock proto era"
-- depending on the era we are in. Look at consensusQueryInEraInMode
singleQuery = createBlockQuery Consensus.GetLedgerTip

--consensusQueryInShelleyBasedEraInCardanoMode
--  :: ShelleyBasedEra era
--  -> Consensus.BlockQuery _ _
--  -> _ --Consensus.Query eraBlock result
--consensusQueryInShelleyBasedEraInCardanoMode sbe =
--  case sbe of
--    ShelleyBasedEraShelley -> Consensus.BlockQuery . Consensus.QueryIfCurrentShelley
--    _ -> undefined

toConsensusQuery
  :: ShelleyBasedEra era
  -> QueryShelleyBasedEra era result
  -> Some (Consensus.Query (Consensus.CardanoBlock Consensus.StandardCrypto))
toConsensusQuery sbe (QueryShelleyBasedEra q)=
  case q of
   QueryEpoch ->
    Some (Consensus.BlockQuery Consensus.GetEpochNo)

   QueryConstitution ->
     caseShelleyToBabbageOrConwayEraOnwards
     (const $ error "toConsensusQueryShelleyBased: QueryConstitution is only available in the Conway era")
     (const $ Some (consensusQueryInEraInMode era Consensus.GetConstitution))
     sbe

   QueryGenesisParameters ->
     Some (consensusQueryInEraInMode era Consensus.GetGenesisConfig)

   QueryProtocolParameters ->
     Some (consensusQueryInEraInMode era Consensus.GetCurrentPParams)

   QueryProtocolParametersUpdate ->
     Some (consensusQueryInEraInMode era Consensus.GetProposedPParamsUpdates)

   QueryStakeDistribution ->
     Some (consensusQueryInEraInMode era Consensus.GetStakeDistribution)

   QueryUTxO QueryUTxOWhole ->
     Some (consensusQueryInEraInMode era Consensus.GetUTxOWhole)

   QueryUTxO (QueryUTxOByAddress addrs) ->
     Some (consensusQueryInEraInMode era (Consensus.GetUTxOByAddress addrs'))
     where
       addrs' :: Set (Shelley.Addr Consensus.StandardCrypto)
       addrs' = toShelleyAddrSet era addrs

   QueryUTxO (QueryUTxOByTxIn txins) ->
     Some (consensusQueryInEraInMode era (Consensus.GetUTxOByTxIn txins'))
     where
       txins' :: Set (Shelley.TxIn Consensus.StandardCrypto)
       txins' = Set.map toShelleyTxIn txins

   QueryStakeAddresses creds _nId ->
     Some (consensusQueryInEraInMode era
             (Consensus.GetFilteredDelegationsAndRewardAccounts creds'))
     where
       creds' :: Set (Shelley.Credential Shelley.Staking Consensus.StandardCrypto)
       creds' = Set.map toShelleyStakeCredential creds

   QueryStakePools ->
     Some (consensusQueryInEraInMode era Consensus.GetStakePools)

   QueryStakePoolParameters poolids ->
     Some (consensusQueryInEraInMode era (Consensus.GetStakePoolParams poolids'))
     where
       poolids' :: Set (Shelley.KeyHash Shelley.StakePool Consensus.StandardCrypto)
       poolids' = Set.map unStakePoolKeyHash poolids

   QueryDebugLedgerState ->
     Some (consensusQueryInEraInMode era (Consensus.GetCBOR Consensus.DebugNewEpochState))

   QueryProtocolState ->
     Some (consensusQueryInEraInMode era (Consensus.GetCBOR Consensus.DebugChainDepState))

   QueryCurrentEpochState ->
     Some (consensusQueryInEraInMode era (Consensus.GetCBOR Consensus.DebugEpochState))

   QueryPoolState poolIds ->
     Some (consensusQueryInEraInMode era (Consensus.GetCBOR (Consensus.GetPoolState (Set.map unStakePoolKeyHash <$> poolIds))))

   QueryStakeSnapshot mPoolIds ->
     Some (consensusQueryInEraInMode era (Consensus.GetCBOR (Consensus.GetStakeSnapshots (fmap (Set.map unStakePoolKeyHash) mPoolIds))))

   QueryPoolDistribution poolIds ->
     Some (consensusQueryInEraInMode era (Consensus.GetCBOR (Consensus.GetPoolDistr (getPoolIds <$> poolIds))))
     where
       getPoolIds :: Set PoolId -> Set (Shelley.KeyHash Shelley.StakePool Consensus.StandardCrypto)
       getPoolIds = Set.map (\(StakePoolKeyHash kh) -> kh)

   QueryStakeDelegDeposits creds ->
     Some (consensusQueryInEraInMode era (Consensus.GetStakeDelegDeposits creds'))
     where
       creds' = Set.map toShelleyStakeCredential creds

   QueryAccountState ->
     Some (consensusQueryInEraInMode era Consensus.GetAccountState)

   QueryGovState ->
     Some (consensusQueryInEraInMode era Consensus.GetGovState)

   QueryDRepState creds ->
     caseShelleyToBabbageOrConwayEraOnwards
     (const $ error "toConsensusQueryShelleyBased: QueryDRepState is only available in the Conway era")
     (const $ Some (consensusQueryInEraInMode era (Consensus.GetDRepState creds)))
     sbe

   QueryDRepStakeDistr dreps ->
     caseShelleyToBabbageOrConwayEraOnwards
     (const $ error "toConsensusQueryShelleyBased: QueryDRepStakeDistr is only available in the Conway era")
     (const $ Some (consensusQueryInEraInMode era (Consensus.GetDRepStakeDistr dreps)))
     sbe

   QueryCommitteeMembersState coldCreds hotCreds statuses ->
     caseShelleyToBabbageOrConwayEraOnwards
     (const $ error "toConsensusQueryShelleyBased: QueryCommitteeMembersState is only available in the Conway era")
     (const $ Some (consensusQueryInEraInMode era (Consensus.GetCommitteeMembersState coldCreds hotCreds statuses)))
     sbe

   QueryStakeVoteDelegatees creds ->
     caseShelleyToBabbageOrConwayEraOnwards
     (const $ error "toConsensusQueryShelleyBased: QueryStakeVoteDelegatees is only available in the Conway era")
     (const $ Some (consensusQueryInEraInMode era
             (Consensus.GetFilteredVoteDelegatees creds')))
     sbe
    where
      creds' :: Set (Shelley.Credential Shelley.Staking Consensus.StandardCrypto)
      creds' = Set.map toShelleyStakeCredential creds

  where
    era = shelleyBasedToCardanoEra sbe

fromConsensusQuery
  :: QueryShelleyBasedEra era result
  -> Consensus.Query (Consensus.CardanoBlock Consensus.StandardCrypto) result'
  -> result'
  -> result
fromConsensusQuery = undefined

type family ConsensusBlockForShelleyBasedEra era where
  ConsensusBlockForShelleyBasedEra (ShelleyBasedEra ShelleyEra) = Consensus.StandardShelleyBlock
  ConsensusBlockForShelleyBasedEra (ShelleyBasedEra AllegraEra) = Consensus.StandardAllegraBlock
  ConsensusBlockForShelleyBasedEra (ShelleyBasedEra MaryEra)    = Consensus.StandardMaryBlock
  ConsensusBlockForShelleyBasedEra (ShelleyBasedEra AlonzoEra)  = Consensus.StandardAlonzoBlock
  ConsensusBlockForShelleyBasedEra (ShelleyBasedEra BabbageEra) = Consensus.StandardBabbageBlock
  ConsensusBlockForShelleyBasedEra (ShelleyBasedEra ConwayEra) = Consensus.StandardConwayBlock
