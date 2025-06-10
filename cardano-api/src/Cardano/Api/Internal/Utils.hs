{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Internal utils for the other Api modules
module Cardano.Api.Internal.Utils
  ( (?!)
  , (?!.)
  , (<<$>>)
  , (<<<$>>>)
  , noInlineMaybeToStrictMaybe
  )
where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Shelley ()

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
