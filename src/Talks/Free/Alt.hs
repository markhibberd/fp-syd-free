module Talks.Free.Alt where

import Control.Applicative

data Freee f a =
    Return a
  | Suspend (f (Freee f a))

instance Functor f => Functor (Freee f) where
  fmap f = go where
    go (Return a)  = Return (f a)
    go (Suspend fa) = Suspend (go <$> fa)

instance Functor f => Monad (Freee f) where
  return = Return
  Return a >>= f = f a
  Suspend a >>= f = Suspend ((>>= f) <$> a)

type FreeList a = Freee ((,) a)

type FreePartial = Freee Maybe
