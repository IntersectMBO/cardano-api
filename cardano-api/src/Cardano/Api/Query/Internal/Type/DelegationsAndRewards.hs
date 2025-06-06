module Cardano.Api.Query.Internal.Type.DelegationsAndRewards
  ( DelegationsAndRewards (..)
  , mergeDelegsAndRewards
  )
where

import Cardano.Api.Address
import Cardano.Api.Certificate.Internal

import Cardano.Ledger.Coin qualified as L

import Data.Aeson as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import GHC.Exts (IsList (..))

-- | A mapping of Shelley reward accounts to both the stake pool that they
-- delegate to and their reward account balance.
-- TODO: Move to cardano-api
newtype DelegationsAndRewards
  = DelegationsAndRewards (Map StakeAddress L.Coin, Map StakeAddress PoolId)
  deriving (Eq, Show)

instance ToJSON DelegationsAndRewards where
  toJSON delegsAndRwds =
    Aeson.Array
      . fromList
      . map delegAndRwdToJson
      $ mergeDelegsAndRewards delegsAndRwds
   where
    delegAndRwdToJson :: (StakeAddress, Maybe L.Coin, Maybe PoolId) -> Aeson.Value
    delegAndRwdToJson (addr, mRewards, mPoolId) =
      Aeson.object
        [ "address" .= addr
        , "delegation" .= mPoolId
        , "rewardAccountBalance" .= mRewards
        ]

instance FromJSON DelegationsAndRewards where
  parseJSON = withArray "DelegationsAndRewards" $ \arr -> do
    let vals = toList arr
    decoded <- mapM decodeObject vals
    pure $ zipper decoded
   where
    zipper
      :: [(StakeAddress, Maybe L.Coin, Maybe PoolId)]
      -> DelegationsAndRewards
    zipper l = do
      let maps =
            [ ( maybe mempty (Map.singleton sa) delegAmt
              , maybe mempty (Map.singleton sa) mPool
              )
            | (sa, delegAmt, mPool) <- l
            ]
      DelegationsAndRewards $
        foldl
          (\(amtA, delegA) (amtB, delegB) -> (amtA <> amtB, delegA <> delegB))
          (mempty, mempty)
          maps

    decodeObject
      :: Aeson.Value
      -> Aeson.Parser (StakeAddress, Maybe L.Coin, Maybe PoolId)
    decodeObject = withObject "DelegationsAndRewards" $ \o -> do
      address <- o .: "address"
      delegation <- o .:? "delegation"
      rewardAccountBalance <- o .:? "rewardAccountBalance"
      pure (address, rewardAccountBalance, delegation)

mergeDelegsAndRewards :: DelegationsAndRewards -> [(StakeAddress, Maybe L.Coin, Maybe PoolId)]
mergeDelegsAndRewards (DelegationsAndRewards (rewardsMap, delegMap)) =
  [ (stakeAddr, Map.lookup stakeAddr rewardsMap, Map.lookup stakeAddr delegMap)
  | stakeAddr <- nub $ Map.keys rewardsMap ++ Map.keys delegMap
  ]
