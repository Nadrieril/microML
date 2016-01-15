module Main where

import Parse
import Eval

main :: IO ()
main = do
    code <- readFile "tests/01.ml"
    let res = parseML code
    print res
    case res of
      Right ast -> print $ eval ast
      _ -> return ()
