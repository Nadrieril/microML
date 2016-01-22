module Main where

import Parse
import EvalAST

main :: IO ()
main = do
    putStrLn ""
    code <- readFile "tests/01.ml"
    let res = parseML code
    case res of
      Right ast -> do
          print ast
          putStrLn $ "-> " ++ show (eval ast)
      Left err -> putStrLn $ "Error: " ++ show err
