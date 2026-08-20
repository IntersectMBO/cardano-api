{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | A cache for a single value. The value is dropped once nothing has read it
-- for a while. Use it for data that is expensive to build but too big or too
-- rarely needed to keep around forever.
module Cardano.Rpc.Server.Internal.TimedCache
  ( TimedCache
  , newTimedCache
  , readThroughCache
  )
where

import RIO

import Data.Time.Clock (DiffTime)

-- | Holds at most one value. The value is dropped once no read has happened
-- for 'expiryTimeout'.
--
-- Create with 'newTimedCache', read with 'readThroughCache'.
data TimedCache a = TimedCache
  { cacheVar :: !(MVar (Maybe (CacheEntry a)))
  -- ^ 'Nothing' means the cache is empty. Otherwise this holds everything the
  -- cache knows, and it is the only mutable cell there is. Keeping the read
  -- time inside the entry rather than beside it is what makes the cache safe:
  -- one lock covers the value, its watcher and its deadline together, so a
  -- reader can no longer refresh the deadline while the watcher is deciding
  -- to drop the value. The 'MVar' is also the lock: when several readers hit
  -- an empty cache at once, one of them loads and the others wait for its
  -- result.
  , expiryTimeout :: !DiffTime
  -- ^ How long the value is kept after the last read.
  }

-- | Everything the cache holds while it is full.
data CacheEntry a = CacheEntry
  { cachedValue :: !a
  -- ^ The cached value.
  , watcher :: !(Async ())
  -- ^ The thread that will drop this value once it goes unread. It lives here
  -- so that a value can never be in the cache without its watcher, and so
  -- that the handle goes away together with the value it watches.
  , lastAccess :: !DiffTime
  -- ^ When the value was last read, from the monotonic clock.
  }

-- | Create an empty cache.
--
-- This starts no thread. The watcher thread only exists while the cache
-- holds a value, so an unused cache holds no data and runs nothing.
newTimedCache
  :: MonadIO m
  => DiffTime
  -- ^ How long the cached value is kept after the last read
  -> m (TimedCache a)
newTimedCache expiryTimeout = do
  cacheVar <- newMVar Nothing
  pure TimedCache{cacheVar, expiryTimeout}

-- | Read the cached value. If the cache is empty, run the load action and
-- cache its result. Every read restarts the expiry timer.
--
-- If the load throws, the exception goes to the caller and the cache stays
-- empty. The next read simply tries again.
readThroughCache
  :: MonadUnliftIO m
  => TimedCache a
  -- ^ The cache to read
  -> m a
  -- ^ How to load the value on a cache miss
  -> m a
readThroughCache TimedCache{cacheVar, expiryTimeout} doLoad =
  modifyMVar cacheVar $ \case
    Just entry@CacheEntry{cachedValue} -> do
      -- Restart the expiry timer. We hold the lock while doing it, so the
      -- watcher cannot be reading the old deadline at the same time.
      now <- getMonotonicDiffTime
      pure (Just entry{lastAccess = now}, cachedValue)
    Nothing -> do
      -- The load runs while we hold the lock, on purpose. When several
      -- readers hit an empty cache at once, the first one loads and the
      -- others block on the lock until the result is stored. This gives one
      -- load in total instead of one load per reader. Do not move the load
      -- out of the lock.
      loaded <- doLoad
      -- Start the timer now that the value is ready. Timing it from when this
      -- reader arrived would let a slow load eat part of the value's lifetime.
      now <- getMonotonicDiffTime
      -- Fork the watcher while still holding the lock, so the value and its
      -- watcher go into the cache together. 'asyncWithUnmask' because a
      -- thread forked inside a 'modifyMVar' callback starts masked, and the
      -- watcher should run unmasked. This is hygiene only: nobody throws to
      -- the watcher, and 'threadDelay' can be interrupted even when masked.
      -- The handle is only stored. Nobody waits on it, links it or cancels
      -- it: the watcher outlives this request and stops by itself.
      watcher <- liftIO $ asyncWithUnmask (\unmask -> unmask watchForExpiry)
      pure (Just CacheEntry{cachedValue = loaded, watcher, lastAccess = now}, loaded)
 where
  -- How much of the value's life is left, given when it was last read and
  -- what the clock says now. Zero or less means it can be dropped.
  remainingIdleTime :: DiffTime -> DiffTime -> DiffTime
  remainingIdleTime lastAccess now = lastAccess + expiryTimeout - now

  -- Sleep until the value has gone unread for 'expiryTimeout', then drop it
  -- and exit.
  --
  -- There is exactly one watcher per cached value, because the watcher is
  -- stored in the entry next to the value it watches. It drops the value at
  -- most once, then exits, taking its own handle with it. Nothing supervises
  -- it and nothing has to: once the cache is empty, no thread is left either.
  watchForExpiry :: IO ()
  watchForExpiry =
    readMVar cacheVar >>= \case
      -- The cache is empty, so there is nothing to watch. Only a watcher
      -- empties the cache, and this one has not, so this should not happen.
      -- Stopping is the right answer if it ever does.
      Nothing -> pure ()
      Just CacheEntry{lastAccess} -> do
        now <- getMonotonicDiffTime
        let remaining = remainingIdleTime lastAccess now
        if remaining > 0
          then do
            -- A read may move the deadline while we sleep, so look at the
            -- entry again instead of dropping the value right after waking up.
            delayFor remaining
            watchForExpiry
          else do
            isEmptied <- modifyMVar cacheVar $ \case
              -- Already empty, so there is nothing left to drop.
              Nothing -> pure (Nothing, True)
              Just entry@CacheEntry{lastAccess = lastAccessUnderLock} -> do
                -- Check again while holding the lock. A read may have
                -- restarted the timer between the check above and us getting
                -- the lock.
                nowUnderLock <- getMonotonicDiffTime
                pure $
                  if remainingIdleTime lastAccessUnderLock nowUnderLock > 0
                    then (Just entry, False)
                    else (Nothing, True)
            unless isEmptied watchForExpiry

-- | The monotonic clock, in seconds since some fixed point.
--
-- The wall clock would be wrong here. An NTP time jump could drop a value
-- right after a read, or keep a stale one alive for hours.
getMonotonicDiffTime :: MonadIO m => m DiffTime
getMonotonicDiffTime = realToFrac <$> getMonotonicTime

-- | Sleep for the given duration, rounded up to whole microseconds.
delayFor :: MonadIO m => DiffTime -> m ()
delayFor duration = threadDelay . ceiling $ duration * 1_000_000
