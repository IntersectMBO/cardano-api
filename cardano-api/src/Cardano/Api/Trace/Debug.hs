{-# LANGUAGE NamedFieldPuns #-}

-- | A collection of utility functions aiding debugging of the code.
module Cardano.Api.Trace.Debug
  ( traceIO'
  , trace'
  , pShow
  )
where

import Control.Monad.IO.Class
import Data.Text.Lazy qualified as TL
import GHC.Stack
import Text.Pretty.Simple (pShow)

import Debug.Trace (traceIO, traceWith)

-- | Trace a value in a 'MonadIO' monad. Colours and renders with indentation the showed vaule.
traceIO'
  :: (HasCallStack, MonadIO m, Show a)
  => String
  -- ^ label for the trace
  -> a
  -- ^ value to trace
  -> m ()
traceIO' l a =
  withFrozenCallStack $
    liftIO . traceIO $
      "\r\n💎💎💎\r\n  "
        <> callsite (getCallStack callStack)
        <> "\r\n📜 "
        <> l
        <> ":\r\n"
        <> TL.unpack (pShow a)
        <> "\r\n"
 where
  callsite ((_, SrcLoc{srcLocFile, srcLocStartLine, srcLocStartCol}) : _) =
    mconcat [srcLocFile, ":", show srcLocStartLine, ":", show srcLocStartCol]
  callsite _ = ""
{-# DEPRECATED traceIO' "traceIO' left in the code" #-}

-- | Trace pure value. Colours and renders with indentation the showed vaule.
trace'
  :: Show a
  => String
  -- ^ label for the trace
  -> a
  -- ^ value to trace
  -> a
trace' l =
  traceWith
    (\x -> "📜 " <> l <> ":\r\n" <> TL.unpack (pShow x))
{-# DEPRECATED trace' "trace' left in the code" #-}
