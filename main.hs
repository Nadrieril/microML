{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module Main where

import Control.Monad (forM_, when)
import System.Directory (getDirectoryContents, doesFileExist)

import qualified AST.Expr (Expr, Name)
import AST.Parse (parseML)
import qualified AST.Eval (eval)

import AFT.Expr (fromAST)
import qualified AFT.Expr (Expr, Name)
import qualified AFT.Eval (eval)

import DeBruijn.Expr (deBruijn)
import qualified DeBruijn.Expr (Expr)
import qualified DeBruijn.Eval (eval)


class Show a => Evaluable a where
    eval :: a -> String

instance Evaluable (AST.Expr.Expr AST.Expr.Name) where
    eval = show . AST.Eval.eval
instance Evaluable (AFT.Expr.Expr AFT.Expr.Name) where
    eval = show . AFT.Eval.eval
instance Evaluable DeBruijn.Expr.Expr where
    eval = show . DeBruijn.Eval.eval

testCode :: Int -> String -> IO ()
testCode stage code = do
    let res = parseML code
    case res of
      Left err -> putStrLn $ "Error: " ++ show err
      Right ast -> do
        when (stage == 0 || stage == 1) $
            printStage ast

        let aft = fromAST ast
        when (stage == 0 || stage == 2) $
            printStage aft

        let dBjn = deBruijn aft
        when (stage == 0 || stage == 3) $
            printStage dBjn

    where printStage tree = do
            print tree
            putStrLn $ "-> " ++ eval tree
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

    where
        testFile stage file = do
            putStrLn $ "-- " ++ file
            code <- readFile file
            testCode stage code
