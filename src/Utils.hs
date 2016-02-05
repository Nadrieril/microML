module Utils where

import Control.Monad.State (StateT, put, get, modify)


type Stack a = [a]

push :: Monad m => a -> StateT (Stack a) m ()
push x = modify (x:)

pop :: Monad m => StateT (Stack a) m a
pop = do
    (x:q) <- get
    put q
    return x

withPush :: Monad m => a -> StateT (Stack a) m b -> StateT (Stack a) m b
withPush x m = local $ push x >> m

local :: Monad m => StateT (Stack a) m b -> StateT (Stack a) m b
local m = do
    old <- get
    ret <- m
    put old
    return ret
