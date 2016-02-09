{-# LANGUAGE RankNTypes, FlexibleContexts #-}
-- ASM = Abstract Stack Machine
module ASM.Instr where

import Control.Eff (Member, Eff, run)
import Control.Eff.State.Strict (State, get, put, modify, evalState)
import Control.Eff.Writer.Strict (Writer, tell, runWriter)

import Common.Expr (Name(..), Value(..), LFixP(..))
import qualified Typed.Expr as Typed (Expr, AbstractExpr(..))


type Id = Int

data BinOp = Plus | Minus | Mult | Div | And | Or | Eq
    deriving (Show)

data Instr =
      Access Id
    | Apply
    | Cur [Instr]
    | Return
    | Let
    | Endlet
    | Branchneg Id
    | Branch Id
    | Op BinOp
    | Push Value
    deriving (Show)


type Env r e = (Member (Writer Instr) r) => Eff r e

tellall :: [Instr] -> Env r ()
tellall = mapM_ tell

getBinOp :: Name -> Instr
getBinOp (Name x) = case x of
    "+" -> bin Plus
    "-" -> bin Minus
    "*" -> bin Mult
    "/" -> bin Div
    "and" -> bin And
    "or" -> bin Or
    "==" -> bin Eq
    x -> error $ "unknown global : " ++ x
    where bin o = Cur [Cur [Access 0, Access 1, Op o]]

compileE :: Typed.Expr -> Env r ()
compileE (expr -> e) = case e of
    Typed.Const c -> tell $ Push c

    Typed.Var x -> tell $ Access x

    Typed.Global x -> tell $ getBinOp x

    Typed.If b e1 e2 -> do
        let c1 = compile e1
        let c2 = compile e2
        compileE b
        tell $ Branchneg (length c1 + 1)
        tellall c1
        tell $ Branch (length c2)
        tellall c2

    Typed.Ap f x -> do
        compileE x
        compileE f
        tell Apply

    Typed.Fun _ e -> tell $ Cur (compile e)

    -- Typed.Fix _ e -> tell $ Fix (compile e)
    Typed.Fix _ _ -> error "cannot compile recursive function"

    Typed.Let _ v e -> do
        compileE v
        tell Let
        compileE e
        tell Endlet

compile :: Typed.Expr -> [Instr]
compile e = fst $ run $
    runWriter (\i -> ([i] ++)) [] $
    compileE e
