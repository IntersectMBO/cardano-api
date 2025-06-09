{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Internal utils for the other Api modules
module Cardano.Api.Internal.Utils
  ( (?!)
  , (?!.)
  , (<<$>>)
  , (<<<$>>>)
  , noInlineMaybeToStrictMaybe

    -- ** CLI option parsing
  , unsafeBoundedRational
  )
where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Shelley ()

import Data.Maybe
import Data.Typeable
import GHC.Stack

(?!) :: Maybe a -> e -> Either e a
Nothing ?! e = Left e
Just x ?! _ = Right x

(?!.) :: Either e a -> (e -> e') -> Either e' a
Left e ?!. f = Left (f e)
Right x ?!. _ = Right x

infixl 4 <<$>>

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

infixl 4 <<<$>>>

(<<<$>>>) :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
(<<<$>>>) = fmap . fmap . fmap

{-# NOINLINE noInlineMaybeToStrictMaybe #-}
noInlineMaybeToStrictMaybe :: Maybe a -> StrictMaybe a
noInlineMaybeToStrictMaybe Nothing = SNothing
noInlineMaybeToStrictMaybe (Just x) = SJust x

-- | Convert Rational to a bounded rational. Throw an exception when the rational is out of bounds.
unsafeBoundedRational
  :: forall r
   . (HasCallStack, Typeable r, BoundedRational r)
  => Rational
  -> r
unsafeBoundedRational x = fromMaybe (error errMessage) $ boundRational x
 where
  errMessage = show (typeRep (Proxy @r)) <> " is out of bounds: " <> show x
