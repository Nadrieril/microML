module Utils where

import Control.Monad.State.Class (MonadState)
import Control.Monad.State (StateT, put, get, modify)
import System.IO.Unsafe (unsafePerformIO)

trace x y = unsafePerformIO (print x >> return y)


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

local :: MonadState s m => m b -> m b
local m = do
    old <- get
    ret <- m
    put old
    return ret
