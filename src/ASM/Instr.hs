{-# LANGUAGE RankNTypes, FlexibleContexts #-}
-- ASM = Abstract Stack Machine
module ASM.Instr where

import qualified Data.Map as M
import Control.Monad (forM)
import Control.Arrow (second)
import Control.Eff (Member, Eff, run)
import Control.Eff.Writer.Strict (Writer, tell, runWriter)
import Control.Eff.Reader.Strict (Reader, ask, runReader)
import Text.Printf (printf)

import Common.Expr (Id, Name, Value(..), LFixP(..))
import qualified DBT.Expr as DBT
import qualified Common.ADT as ADT
import qualified Common.Context as C
import qualified Common.Pattern as P (Pattern(..))


data Instr =
      Access Id
    | Apply
    | Cur [Instr]
    | Rec [Instr]
    | Return
    | Let
    | Endlet
    | Branchneg Int
    | Branchmatch Pattern Int
    | Branch Int
    | SysCall Name
    | Constructor Name Int Int
    | Deconstructor Name Int
    | Push Value
    | Panic String
    deriving (Show, Read)

data Pattern = PVar | Pattern Name [Pattern]
    deriving (Show, Read)

convertPattern :: P.Pattern a -> Pattern
convertPattern (P.PVar _) = PVar
convertPattern (P.Pattern n l) = Pattern n (map convertPattern l)


type Env r e = (
    Member (Reader C.Context) r,
    Member (Writer Instr) r
    ) => Eff r e

tellall :: [Instr] -> Env r ()
tellall = mapM_ tell


getSysCall :: C.Context -> Name -> Instr
getSysCall ctx x | Just (cv, _) <- x `M.lookup` ctx =
    case cv of
        C.Value v -> Push v
        C.Constructor adt n i -> let name = ADT.constructorName (ADT.adtConstructors adt !! n) in
                Constructor name n i
        C.Deconstructor adt i -> Deconstructor (ADT.adtName adt) i
        C.SysCall _ -> error $ printf "Cannot compile syscall from local context: %s" (show x)
getSysCall _ x = SysCall x


compileE :: DBT.TypedExpr -> Env r ()
compileE (expr -> e) = case e of
    DBT.Const c -> tell $ Push c

    DBT.Bound _ x -> tell $ Access x

    DBT.Free x -> do
        ctx <- ask
        tell $ getSysCall ctx x

    DBT.If b e1 e2 -> do
        c1 <- compile' e1
        c2 <- compile' e2
        compileE b
        tell $ Branchneg (length c1 + 1)
        tellall c1
        tell $ Branch (length c2)
        tellall c2

    DBT.Match e l -> do
        let writeCases [] = ([], 0, [], 0)
            writeCases ((name, c) : q) =
                let incr = second . (+) in
                let lc = length c in
                let (matches, lmatches, execs, lexecs) = writeCases q in
                let matches' = (name, lmatches + 1) : map (incr (lc + 1)) matches in
                let execs' = Branch (lc + lexecs) : (c ++ execs) in
                (matches', lmatches + 1, execs', lc + lexecs + 1)

        compileE e
        cases <- forM l $ \(DBT.Scope _ (p, e)) -> do
            c <- compile' e
            return (convertPattern p, c)
        let (matches, _, execs, _) = writeCases cases
        tellall (uncurry Branchmatch <$> matches)
        tellall (Panic "pattern-matching failed" : tail execs)

    DBT.Ap (expr -> DBT.Ap (expr -> DBT.Free "&&") x) y -> do
        compileE x
        cy <- compile' y
        tell $ Branchneg (length cy + 1)
        tellall cy
        tell $ Branch 1
        tell $ Push $ B False

    DBT.Ap (expr -> DBT.Ap (expr -> DBT.Free "||") x) y -> do
        compileE x
        cy <- compile' y
        tell $ Branchneg 2
        tell $ Push $ B True
        tell $ Branch (length cy)
        tellall cy

    DBT.Ap f x -> do
        compileE x
        compileE f
        tell Apply

    DBT.SFun e -> Cur <$> compile' e >>= tell

    DBT.SFix (expr -> DBT.SFun e) -> Rec <$> compile' e >>= tell
    DBT.SFix _ -> error "cannot compile arbitrary recursive definition"

    DBT.SLet v e -> do
        compileE v
        tell Let
        compileE e
        tell Endlet

    _ -> error "impossible"
    where
        compile' :: DBT.TypedExpr -> Env r [Instr]
        compile' e = ask >>= \ctx -> return $ compile (ctx, e)


compile :: DBT.Program -> [Instr]
compile (ctx, e) = fst $ run $
    flip runReader ctx $
    runWriter (\i -> ([i] ++)) [] $
    compileE e
