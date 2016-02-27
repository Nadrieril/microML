{-# LANGUAGE RankNTypes, FlexibleContexts, OverloadedStrings #-}
-- ASM = Abstract Stack Machine
module ASM.Instr where

import qualified Data.Map as M
import Control.Eff (Member, Eff, run)
import Control.Eff.Writer.Strict (Writer, tell, runWriter)
import Text.Printf (printf)

import Common.Expr (Id, Name(..), Value(..), LFixP(..))
import qualified DBT.Expr as DBT
import qualified Common.ADT as ADT
import qualified Common.Context as C


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
    | Constructor Name Int Int
    | Deconstructor Name Int
    | Push Value
    deriving (Show, Read)


type Env r e = (Member (Writer Instr) r) => Eff r e

tellall :: [Instr] -> Env r ()
tellall = mapM_ tell


getSysCall :: C.Context -> Name -> Instr
getSysCall ctx x | Just (cv, _) <- x `M.lookup` ctx =
    case cv of
        C.Value v -> Push v
        C.Constructor adt n i -> let name = ADT.constructorName (ADT.adtConstructors adt !! n) in
                Constructor name n i
        C.Deconstructor adt i -> Deconstructor (ADT.adtName adt) i
        -- C.SysCall _ -> error $ printf "Cannot compile syscall from local context: %s" (show x)
        C.SysCall _ -> SysCall x --TODO: remove
getSysCall _ x = SysCall x


compileE :: DBT.TypedExpr -> Env r ()
compileE (expr -> e) = case e of
    DBT.Const c -> tell $ Push c

    DBT.Bound x -> tell $ Access x

    DBT.Free x -> tell $ getSysCall C.globalContext x

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


compile :: DBT.TypedExpr -> [Instr]
compile e = fst $ run $
    runWriter (\i -> ([i] ++)) [] $
    compileE e
