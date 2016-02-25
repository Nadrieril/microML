{-# LANGUAGE RankNTypes, FlexibleContexts, OverloadedStrings #-}
-- ASM = Abstract Stack Machine
module ASM.Instr where

import Control.Eff (Member, Eff, run)
import Control.Eff.Writer.Strict (Writer, tell, runWriter)

import Common.Expr (Name(..), Value(..), LFixP(..))
import qualified DBT.Expr as DBT


type Id = Int

data Instr =
      Access Id
    | Apply
    | Cur [Instr]
    | Rec [Instr]
    | Return
    | Let
    | Endlet
    | Branchneg Id
    | Branch Id
    | SysCall Name
    | Push Value
    deriving (Show, Read)


type Env r e = (Member (Writer Instr) r) => Eff r e

tellall :: [Instr] -> Env r ()
tellall = mapM_ tell


compileE :: DBT.Expr -> Env r ()
compileE (expr -> e) = case e of
    DBT.Const c -> tell $ Push c

    DBT.Bound x -> tell $ Access x

    DBT.Free x -> tell $ SysCall x

    DBT.If b e1 e2 -> do
        let c1 = compile e1
        let c2 = compile e2
        compileE b
        tell $ Branchneg (length c1 + 1)
        tellall c1
        tell $ Branch (length c2)
        tellall c2

    DBT.Ap (expr -> DBT.Ap (expr -> DBT.Free "&&") x) y -> do
        compileE x
        let cy = compile y
        tell $ Branchneg (length cy + 1)
        tellall cy
        tell $ Branch 1
        tell $ Push $ B False

    DBT.Ap (expr -> DBT.Ap (expr -> DBT.Free "||") x) y -> do
        compileE x
        let cy = compile y
        tell $ Branchneg 2
        tell $ Push $ B True
        tell $ Branch (length cy)
        tellall cy

    DBT.Ap f x -> do
        compileE x
        compileE f
        tell Apply

    DBT.SFun e -> tell $ Cur (compile e)

    DBT.SFix (expr -> DBT.SFun e) -> tell $ Rec (compile e)
    DBT.SFix _ -> error "cannot compile arbitrary recursive definition"

    DBT.SLet v e -> do
        compileE v
        tell Let
        compileE e
        tell Endlet

    _ -> error "impossible"

compile :: DBT.Expr -> [Instr]
compile e = fst $ run $
    runWriter (\i -> ([i] ++)) [] $
    compileE e
