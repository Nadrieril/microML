{-# LANGUAGE RankNTypes, FlexibleContexts #-}
-- ASM = Abstract Stack Machine
module ASM.Instr where

import Control.Eff (Member, Eff, run)
import Control.Eff.Writer.Strict (Writer, tell, runWriter)

import Common.Expr (Name(..), Value(..), SysCall(..), LFixP(..))
import qualified Common.StdLib as Std (getSysCall)
import qualified Typed.Expr as Typed (Expr, AbstractExpr(..))


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
    | SysCall SysCall
    | Push Value
    deriving (Show)


type Env r e = (Member (Writer Instr) r) => Eff r e

tellall :: [Instr] -> Env r ()
tellall = mapM_ tell

getSysCall :: Name -> Instr
getSysCall x = Cur [Cur [Access 0, Access 1, SysCall (Std.getSysCall x)]]

compileE :: Typed.Expr -> Env r ()
compileE (expr -> e) = case e of
    Typed.Const c -> tell $ Push c

    Typed.Var x -> tell $ Access x

    Typed.Global x -> tell $ getSysCall x

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

    Typed.Fix _ (expr -> Typed.Fun _ e) -> tell $ Rec (compile e)
    Typed.Fix _ _ -> error "cannot compile arbitrary recursive definition"

    Typed.Let _ v e -> do
        compileE v
        tell Let
        compileE e
        tell Endlet

compile :: Typed.Expr -> [Instr]
compile e = fst $ run $
    runWriter (\i -> ([i] ++)) [] $
    compileE e
