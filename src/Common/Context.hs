{-# LANGUAGE ParallelListComp #-}
module Common.Context (
      Context
    , ContextValue(..)
    , contextFromADTs
    ) where

import qualified Data.Map as M

import Common.Expr (Id, Name, Value)
import Common.ADT hiding (Constructor)
import Common.Type


data ContextValue =
      Value Value
    | Constructor (ADT Id) Id Int
    | SysCall (Value -> ContextValue)

type Context = M.Map Name (ContextValue, Type)


adtContext :: ADT Id -> Context
adtContext adt = M.fromList [ (cname, (Constructor adt i carity, ct))
                    | (cname, ct, carity) <- constructorsInfo adt
                    | i <- [0..] ]

contextFromADTs :: [ADT Name] -> Context
contextFromADTs adts = mconcat (map (adtContext . bindTypeVars) adts)
