module Main where

import AST.Parse (parseML)
import qualified AST.Eval (eval)
import AFT.Expr (fromAST)
import qualified AFT.Eval (eval)
import DeBruijn.Expr (deBruijn)
import qualified DeBruijn.Eval (eval)

main :: IO ()
main = do
    putStrLn ""
    code <- readFile "tests/01.ml"
    let res = parseML code
    case res of
      Left err -> putStrLn $ "Error: " ++ show err
      Right ast -> do
          print ast
          putStrLn $ "-> " ++ show (AST.Eval.eval ast)
          putStrLn ""

          let aft = fromAST ast
          print aft
          putStrLn $ "-> " ++ show (AFT.Eval.eval aft)
          putStrLn ""

          let dBjn = deBruijn aft
          print dBjn
          putStrLn $ "-> " ++ show (DeBruijn.Eval.eval dBjn)
