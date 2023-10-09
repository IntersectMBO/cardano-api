{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.Governance.Vote
  ( GovernanceVoteCmds(..)
  , AnyVoteViewCmd(..)
  , renderGovernanceVoteCmds
  ) where


import           Cardano.Api.Shelley

import           Cardano.CLI.Types.Governance

import           Data.Text (Text)

data GovernanceVoteCmds era
  = GovernanceVoteCreateCmd
      AnyVote
  | GovernanceVoteViewCmd
      (AnyVoteViewCmd era)

data AnyVoteViewCmd era
  = AnyVoteViewCmd
    { governanceVoteViewCmdYamlOutput :: Bool
    , governanceVoteViewCmdEra :: ConwayEraOnwards era
    , governanceVoteViewCmdVoteFile :: VoteFile In
    , governanceVoteViewCmdOutputFile :: Maybe (File () Out)
    }


renderGovernanceVoteCmds :: ()
  => GovernanceVoteCmds era
  -> Text
renderGovernanceVoteCmds = \case
  GovernanceVoteCreateCmd {} ->
    "governance vote create"
  GovernanceVoteViewCmd {} ->
    "governance vote view"