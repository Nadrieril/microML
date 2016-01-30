{-# LANGUAGE FlexibleContexts #-}
module Utils where

import Control.Monad.State.Class (MonadState, put, get, modify)
import Control.Monad.State (StateT)
import System.IO.Unsafe (unsafePerformIO)

debug = False

trace x y = if debug
    then unsafePerformIO (print x >> return y)
    else y


type Stack a = [a]

push :: MonadState (Stack a) m => a -> m ()
push x = modify (x:)

pop :: MonadState (Stack a) m => m a
pop = do
    (x:q) <- get
    put q
    return x

withPush :: Monad m => a -> StateT (Stack a) m b -> StateT (Stack a) m b
withPush x m = local $ push x >> m

local :: MonadState s m => m b -> m b
local m = do
    old <- get
    ret <- m
    put old
    return ret
