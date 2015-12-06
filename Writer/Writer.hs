module Monads.Writer

where

import Control.Applicative

import Data.Monoid

newtype Writer w a = Writer { runWriter :: (a,w) }

instance Functor (Writer w) where
    fmap f c = Writer (f a, w)
        where (a,w) = runWriter c

instance (Monoid w) => Applicative (Writer w) where
    pure a = Writer (a, mempty)
    c1 <*> c2 = Writer (f a, w1 <> w2)
        where (f, w1) = runWriter c1
              (a, w2) = runWriter c2

instance (Monoid w) => Monad (Writer w) where
    return = pure
    c >>= f = Writer (b, w1 <> w2)
        where (a, w1) = runWriter c
              (b, w2) = runWriter $ f a

writer :: (a,w) -> Writer w a
writer = Writer

tell :: w -> Writer w ()
tell w = writer ((),w)

listen :: Writer w a -> Writer w (a, w)
listen c = Writer ((a,w), w)
  where (a,w) = runWriter c


pass :: Writer w (a, w -> w) -> Writer w a
pass c = Writer (a, f w)
    where ((a, f), w) = runWriter c

listens :: (w -> b) -> Writer w a -> Writer w (a,b)
listens f c = Writer ((a, f w), w)
    where (a, w) = runWriter c

censor :: (w -> w) -> Writer w a -> Writer w a
censor f c = Writer (a, f w)
    where (a, w) = runWriter c

execWriter :: Writer w a -> w
execWriter = snd . runWriter


mapWriter :: ((a, w) -> (b, w')) -> Writer w a -> Writer w' b
mapWriter f = writer . f . runWriter
