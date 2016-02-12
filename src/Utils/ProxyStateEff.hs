{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Utils.ProxyStateEff (get, put, modify) where

import Control.Eff (Member, Eff)
import Control.Eff.State.Strict (State)
import qualified Control.Eff.State.Strict as State (get, put, modify)

import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)

get :: (Typeable a, Member (State a) r) => Proxy a -> Eff r a
get _ = State.get

put :: (Typeable a, Member (State a) r) => Proxy a -> a -> Eff r ()
put _ = State.put

modify :: (Typeable a, Member (State a) r) => Proxy a -> (a -> a) -> Eff r ()
modify _ = State.modify
