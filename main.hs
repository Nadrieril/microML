module Main where

import Parse (parseML)
import qualified EvalAST (eval)
import qualified EvalAFT (eval)
import IR.AFT (fromAST)
import IR.DeBruijn (deBruijn)

main :: IO ()
main = do
    putStrLn ""
    code <- readFile "tests/01.ml"
    let res = parseML code
    case res of
      Left err -> putStrLn $ "Error: " ++ show err
      Right ast -> do
          print ast
          putStrLn $ "-> " ++ show (EvalAST.eval ast)
          putStrLn ""

          let aft = fromAST ast
          print aft
          putStrLn $ "-> " ++ show (EvalAFT.eval aft)
          putStrLn ""

          let dBjn = deBruijn aft
          print dBjn
