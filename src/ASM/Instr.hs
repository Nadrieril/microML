{-# LANGUAGE RankNTypes, FlexibleContexts #-}
-- ASM = Abstract Stack Machine
module ASM.Instr where

import Control.Eff (Member, Eff, run)
import Control.Eff.Writer.Strict (Writer, tell, runWriter)

import Common.Expr (Name(..), Value(..), LFixP(..))
import qualified DBT.Expr as DBT (Expr, AbstractExpr(..))


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
    deriving (Show)


type Env r e = (Member (Writer Instr) r) => Eff r e

tellall :: [Instr] -> Env r ()
tellall = mapM_ tell


compileE :: DBT.Expr -> Env r ()
compileE (expr -> e) = case e of
    DBT.Const c -> tell $ Push c

    DBT.Var x -> tell $ Access x

    DBT.Global x -> tell $ SysCall x

    DBT.If b e1 e2 -> do
        let c1 = compile e1
        let c2 = compile e2
        compileE b
        tell $ Branchneg (length c1 + 1)
        tellall c1
        tell $ Branch (length c2)
        tellall c2

    DBT.Ap f x -> do
        compileE x
        compileE f
        tell Apply

    DBT.Fun _ e -> tell $ Cur (compile e)

    DBT.Fix _ (expr -> DBT.Fun _ e) -> tell $ Rec (compile e)
    DBT.Fix _ _ -> error "cannot compile arbitrary recursive definition"

    DBT.Let _ v e -> do
        compileE v
        tell Let
        compileE e
        tell Endlet

compile :: DBT.Expr -> [Instr]
compile e = fst $ run $
    runWriter (\i -> ([i] ++)) [] $
    compileE e
