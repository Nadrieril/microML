{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, FlexibleInstances #-}
module Utils.Stackable where

import Data.Maybe (fromJust)
import Data.Typeable (Typeable)
import Data.Proxy (Proxy(..))
import Control.Eff (Member, Eff)
import Control.Eff.State.Strict (State)

import Utils.ProxyStateEff

type Stack a = [a]

class Stackable b a | a -> b where
    wrapStack :: Stack b -> a
    unwrapStack :: a -> Stack b

instance Stackable a (Stack a) where
    wrapStack = id
    unwrapStack = id

inStack :: Stackable b a => (Stack b -> Stack b) -> a -> a
inStack = (wrapStack .) . (. unwrapStack)

push :: (Stackable b a, Typeable a, Member (State a) r) => Proxy a -> b -> Eff r ()
push p x = modify p (wrapStack . (x:) . unwrapStack)

popM :: (Stackable b a, Typeable a, Member (State a) r) => Proxy a -> Eff r (Maybe b)
popM p = do
    xs <- get p
    case unwrapStack xs of
        [] -> return Nothing
        x:xs -> put p (wrapStack xs) >> return (Just x)

pop :: (Stackable b a, Typeable a, Member (State a) r) => Proxy a -> Eff r b
pop p = fromJust <$> popM p
