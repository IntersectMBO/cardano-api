{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for how @ReadGenesis@ builds its response from a field mask.
module Test.Cardano.Rpc.ReadGenesis where

import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c
import Cardano.Rpc.Server.Internal.FieldMask
import Cardano.Rpc.Server.Internal.UtxoRpc.Query
  ( readGenesisFill
  )

import Cardano.Ledger.Shelley.Genesis qualified as L (ShelleyGenesis (..))

import RIO
import RIO.Writer (runWriter)

import Data.ProtoLens (defMessage)

import Test.Cardano.Rpc.Genesis.Fixture (decodeFixtureAs, shelleyFixturePath)
import Test.Cardano.Rpc.RecordedQuery

import Hedgehog as H
import Hedgehog.Extras qualified as H

hprop_field_mask_read_genesis_hash_only_reads_no_genesis_file :: Property
hprop_field_mask_read_genesis_hash_only_reads_no_genesis_file = H.propertyOnce $ do
  (response, ran) <- H.evalNF . build ["genesis"] (error "forced") =<< shelleyGenesis
  ran === []
  response === (defMessage & U5c.genesis .~ realHash)

hprop_field_mask_read_genesis_caip2_only_reads_the_file_and_skips_the_conversion :: Property
hprop_field_mask_read_genesis_caip2_only_reads_the_file_and_skips_the_conversion = H.propertyOnce $ do
  (response, ran) <- H.evalNF . build ["caip2"] (error "forced") =<< shelleyGenesis
  ran === ["shelley"]
  response === (defMessage & U5c.caip2 .~ "cardano:42")

hprop_field_mask_read_genesis_cardano_only_skips_the_identity_fields :: Property
hprop_field_mask_read_genesis_cardano_only_skips_the_identity_fields = H.propertyOnce $ do
  (response, ran) <- H.evalNF . build ["cardano"] (const realCardano) =<< shelleyGenesis
  ran === ["shelley"]
  response === (defMessage & U5c.cardano .~ realCardano)

hprop_field_mask_read_genesis_nothing_selected_runs_no_query :: Property
hprop_field_mask_read_genesis_nothing_selected_runs_no_query = H.propertyOnce $ do
  (response, ran) <- H.evalNF . build ["bogus"] (error "forced") =<< shelleyGenesis
  (response, ran) === (defMessage, [])

hprop_field_mask_read_genesis_select_all_reads_the_file_once :: Property
hprop_field_mask_read_genesis_select_all_reads_the_file_once = H.propertyOnce $ do
  (response, ran) <- build [] (const realCardano) <$> shelleyGenesis
  ran === ["shelley"]
  response === expectedResponse

hprop_field_mask_read_genesis_nested_path_trims_cardano :: Property
hprop_field_mask_read_genesis_nested_path_trims_cardano = H.propertyOnce $ do
  (response, _) <- build ["cardano.network_magic"] (const realCardano) <$> shelleyGenesis
  response === (defMessage & U5c.cardano . U5c.networkMagic .~ 42)

-- | Run the response fill for the mask paths, recording each read of the genesis file.
build
  :: [FieldPath]
  -- ^ Field mask paths, rooted at the response
  -> (L.ShelleyGenesis -> U5c.Genesis)
  -- ^ Converts the Shelley genesis to the @cardano@ field
  -> L.ShelleyGenesis
  -- ^ The Shelley genesis that the recorded read returns
  -> (U5c.ReadGenesisResponse, [Text])
build = buildWith . resolve

-- | 'build' for a resolved field mask. The handler runs the fill without a session.
buildWith
  :: FieldMaskTree U5c.ReadGenesisResponse
  -- ^ The field mask, rooted at the response
  -> (L.ShelleyGenesis -> U5c.Genesis)
  -- ^ Converts the Shelley genesis to the @cardano@ field
  -> L.ShelleyGenesis
  -- ^ The Shelley genesis that the recorded read returns
  -> (U5c.ReadGenesisResponse, [Text])
buildWith fieldMask toCardano genesis =
  runWriter . runFill id fieldMask $ readGenesisFill realHash (recorded "shelley" genesis) toCardano

expectedResponse :: U5c.ReadGenesisResponse
expectedResponse =
  defMessage
    & U5c.genesis .~ realHash
    & U5c.caip2 .~ "cardano:42"
    & U5c.cardano .~ realCardano

-- | The Shelley genesis fixture with a network magic that is no well-known network's.
shelleyGenesis :: (MonadTest m, MonadIO m) => m L.ShelleyGenesis
shelleyGenesis = do
  genesis <- decodeFixtureAs @L.ShelleyGenesis shelleyFixturePath
  pure genesis{L.sgNetworkMagic = 42}

realHash :: ByteString
realHash = "genesis-hash"

realCardano :: U5c.Genesis
realCardano = defMessage & U5c.networkMagic .~ 42 & U5c.epochLength .~ 7
