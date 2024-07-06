module Cardano.Api.Rewards
  ( DelegationsAndRewards (..)
  , mergeDelegsAndRewards
  )
where

import Cardano.Api.Address
import Cardano.Api.Certificate
import qualified Cardano.Ledger.Coin as L
import Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector

-- | A mapping of Shelley reward accounts to both the stake pool that they
-- delegate to and their reward account balance.
-- TODO: Move to cardano-api
newtype DelegationsAndRewards
  = DelegationsAndRewards (Map StakeAddress L.Coin, Map StakeAddress PoolId)
  deriving (Eq, Show)

instance ToJSON DelegationsAndRewards where
  toJSON delegsAndRwds =
    Aeson.Array
      . Vector.fromList
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
    let vals = Vector.toList arr
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
