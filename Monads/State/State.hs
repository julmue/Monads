module Monad.State
    ( State(State)
    , get
    , put
    , modify
    , gets
    , state
    , evalState
    , execState
    , withState
    ) where

import Control.Applicative
import Control.Monad


newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
--  fmap :: (a -> b) -> State s a -> State s b
    fmap f c = State $ \s ->
        let (a, s') = runState c s
        in (f a, s')

instance Applicative (State s) where
--  pure :: a -> State s a
    pure x = State $ \s -> (x, s)
--  (<*>) :: State s a -> State s a -> State s a
    cf <*> ca = State $ \s ->
        let (f, s') = runState cf s
            (a, s'') = runState ca s'
        in (f a, s'')

instance Monad (State s) where
    return = pure
--  (>>=) :: State s a -> (a -> State s b) -> State s b
    ca >>= f = State $ \s ->
        let (a, s') = runState ca s
        in runState (f a) s'

get :: State s s
get = state $ \s -> (s,s)

put :: s -> State s ()
put s = state $ \_ -> ((),s)

modify :: (s -> s) -> State s ()
modify f = state $ \s -> ((), f s)

gets :: (s -> a) -> State s a
gets f = do
    s <- get
    return $ f s

state :: (s -> (a, s)) -> State s a
state = State

evalState :: State s a -> s -> a
evalState c = fst . runState c

execState :: State s a -> s -> s
execState c = snd . runState c

withState :: (s -> s) -> State s a -> State s a
withState f c = modify f >> c


