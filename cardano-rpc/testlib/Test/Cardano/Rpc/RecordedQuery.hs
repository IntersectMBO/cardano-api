{-# LANGUAGE OverloadedStrings #-}

-- | Actions that record their name when they run, for testing which queries a
-- field fill runs.
module Test.Cardano.Rpc.RecordedQuery where

import RIO
import RIO.Writer (Writer, tell)

-- | An action that records its name when it runs.
recorded
  :: Text
  -- ^ The name to record
  -> a
  -- ^ The result of the action
  -> Writer [Text] a
recorded name value = tell [name] $> value

-- | A runner that records each session it opens.
session
  :: Writer [Text] a
  -- ^ The actions to run in the session
  -> Writer [Text] a
session = (tell ["session"] *>)
