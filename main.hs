{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module Main where

import Control.Monad (forM_, when)
-- import Control.Exception (throwIO, IOException)
import Control.Exception
import System.Directory (getDirectoryContents, doesFileExist)

import qualified AST.Expr (Expr)
import AST.Parse (parseML)

import AFT.Expr (fromAST)
import qualified AFT.Expr (Expr)

import DeBruijn.Expr (deBruijn)
import qualified DeBruijn.Expr (Expr)
import qualified DeBruijn.Eval (eval)

import Typed.Infer (inferType)
import qualified Typed.Expr (Expr)

import qualified ASM.Eval as ASM


class Show a => Evaluable a where
    eval :: a -> String

instance Evaluable AST.Expr.Expr where
    eval = return "<no evaluation>"
instance Evaluable AFT.Expr.Expr where
    eval = return "<no evaluation>"
instance Evaluable DeBruijn.Expr.Expr where
    eval = show . DeBruijn.Eval.eval
instance Evaluable Typed.Expr.Expr where
    eval = return "<no evaluation>"

testCode :: Int -> String -> IO ()
testCode stage code =
    case parseML code of
      Left err -> putStrLn $ "Error: " ++ show err
    --   Left err -> fail (show err)
      Right ast -> do
        printStage 1 stage ast

        let aft = fromAST ast
        printStage 2 stage aft

        let dBjn = deBruijn aft
        printStage 3 stage dBjn

        let typed = inferType dBjn
        printStage 4 stage typed

    where printStage i stage tree =
            when (stage == 0 || stage == i) $ do
                print tree
                catch
                    (putStrLn $ "-> " ++ eval tree)
                    (\(err::SomeException) -> putStrLn $ "Error: " ++ show err)
                putStrLn ""



main :: IO ()
main = do
    putStrLn ""
    files <- getDirectoryContents "tests"
    forM_ files $ \name -> do
        let file = "tests/" ++ name
        isFile <- doesFileExist file
        when (isFile && name /= "test.ml") $
            testFile 3 file

    putStrLn ""
    testFile 0 "tests/test.ml"

    print $ ASM.eval ASM.example

    where
        testFile stage file = do
            putStrLn $ "-- " ++ file
            code <- readFile file
            testCode stage code
