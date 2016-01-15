module Main where

import Parse

main :: IO ()
main = do
    code <- readFile "tests/01.ml"
    putStrLn . show $ parseML code
