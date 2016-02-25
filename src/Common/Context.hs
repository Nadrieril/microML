{-# LANGUAGE ParallelListComp #-}
module Common.Context where

import Data.Monoid
import qualified Data.Map as M
import Control.Arrow

import Common.Expr (Name, Value)
import qualified Common.StdLib as Std
import Common.ADT hiding (Id, Constructor)
import Common.Type

type Id = Int

data ContextValue =
      Value Value
    | Constructor (ADT Id) Id Int
    | Deconstructor (ADT Id) Int
    | SysCall (Value -> Std.StdLibValue)

type Context = M.Map Name (ContextValue, Type)


adtContext :: ADT Id -> Context
adtContext adt =
    let (decname, dect, decarity) = deconstructorInfo adt in
    let dec = (decname, (Deconstructor adt decarity, dect)) in
    let decs = [(cname, (Constructor adt i carity, ct))
                    | (cname, ct, carity) <- constructorsInfo adt
                    | i <- [0..]] in
    M.fromList $ dec : decs

stdContext :: Context
stdContext =
    let f (Std.Val v) = Value v
        f (Std.Fun f) = SysCall f in
    let sctv = Std.sysCallToValue . Std.getSysCall in
    let sctt = Std.sysCallToType . Std.getSysCall in
    M.fromList $ map (id &&& (f . sctv &&& sctt)) Std.sysCalls

globalContext :: Context
globalContext = stdContext <> mconcat (map adtContext adts)
