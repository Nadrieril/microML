{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module Main where

-- import Control.Monad (forM_)
import Control.Monad (when)
-- import Control.Exception (throwIO, IOException)
import Control.Exception
-- import System.Directory (getDirectoryContents, doesFileExist)

import qualified AST.Expr (Expr)
import AST.Parse (parseML)

import AFT.Expr (fromAST)
import qualified AFT.Expr (Expr)

import DBT.Expr (deBruijn)
import qualified DBT.Expr (Expr, TypedExpr)
import qualified DBT.Eval (eval)
import DBT.Infer (inferType)

import qualified ASM.Instr as ASM
import qualified ASM.Eval as ASM
import Common.ADT

class Show a => Evaluable a where
    eval :: a -> String

instance Evaluable AST.Expr.Expr where
    eval = return "<no evaluation>"
instance Evaluable AFT.Expr.Expr where
    eval = return "<no evaluation>"
instance Evaluable DBT.Expr.Expr where
    eval = show . DBT.Eval.eval
instance Evaluable DBT.Expr.TypedExpr where
    eval = return "<no evaluation>"
instance Evaluable [ASM.Instr] where
    eval = show . ASM.eval . ASM.Code

testCode :: Int -> String -> IO ()
testCode stage code =
    case parseML code of
      Left err -> putStrLn $ "Error: " ++ show err
    --   Left err -> fail (show err)
      Right ast -> do
        -- printStage 1 stage ast

        let aft = fromAST ast
        -- printStage 2 stage aft

        let dbt = deBruijn aft
        -- printStage 3 stage dbt

        let typed = inferType dbt
        -- printStage 4 stage typed
        print typed
        putStrLn ""
        evalStage dbt

        -- let compiled = ASM.compile dbt
        -- printStage 5 stage compiled

    where printStage i stage tree =
            when (stage == 0 || stage == i) $ do
                print tree
                evalStage tree
                putStrLn ""
          evalStage tree =
                catch (putStrLn $ "-> " ++ eval tree)
                      (\(err::SomeException) -> putStrLn $ "Error: " ++ show err)



main :: IO ()
main = do
    -- putStrLn ""
    -- files <- getDirectoryContents "tests"
    -- forM_ files $ \name -> do
    --     let file = "tests/" ++ name
    --     isFile <- doesFileExist file
    --     when (isFile && name /= "test.ml") $
    --         testFile 3 file

    putStrLn ""
    testFile 0 "tests/test.ml"

    where
        testFile stage file = do
            putStrLn $ "-- " ++ file
            code <- readFile file
            testCode stage code
