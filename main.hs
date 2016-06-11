{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module Main where

import Control.Monad
import Control.Exception
import Options

import AST.Parse (parseML)
import AFT.Expr (fromAST)
import DBT.Expr (fromAFT, Program)
import qualified DBT.Eval (eval)
import DBT.Infer (inferType)

import qualified ASM.Instr as ASM
import qualified ASM.Eval as ASM
import Common.Expr (pprint)
import Common.Context (contextFromADTs)


getFileContents :: FilePath -> IO String
getFileContents file = if file == "-" then getContents else readFile file

processFile :: Bool -> FilePath -> IO Program
processFile showTyped file = do
    code <- getFileContents file
    (adts, ast) <- evalerr "Parse error" $ either (error.show) id $ parseML code
    aft <- evaluate $ fromAST ast
    dbt <- evaluate $ fromAFT aft
    ctx <- evaluate $ contextFromADTs adts
    (errors, typed) <- evalerr "Type error" $ inferType ctx dbt

    when showTyped $ do
        putStrLn $ let ?toplevel = True in pprint typed
        putStrLn ""

    unless (null errors) $
        error $ unlines $ "Errors encountered while inferring types:" : map (("  "++) . show) errors

    return (ctx, typed)

    where evalerr n x =
                catch (evaluate x)
                      (\(err::ErrorCall) -> error $ n ++ ": " ++ show err)


data MainOptions = MainOptions
instance Options MainOptions where
    defineOptions = pure MainOptions

data CompileOpts = CompileOpts Bool
instance Options CompileOpts where
    defineOptions = pure CompileOpts
        <*> simpleOption "show" False "enable/disable parsed expression display"

data TypeOptions = TypeOptions
instance Options TypeOptions where
    defineOptions = pure TypeOptions

data EvalOptions = EvalOptions Bool
instance Options EvalOptions where
    defineOptions = pure EvalOptions
        <*> simpleOption "show" False "enable/disable parsed expression display"

data RunOptions = RunOptions Bool
instance Options RunOptions where
    defineOptions = pure RunOptions
        <*> simpleOption "trace" False "enable/disable trace display"


compileCmd :: MainOptions -> CompileOpts -> [String] -> IO ()
compileCmd _ (CompileOpts optShow) args = do
    let [file] = args
    program <- processFile optShow file

    let compiled = ASM.compile program
    print compiled


typeCmd :: MainOptions -> TypeOptions -> [String] -> IO ()
typeCmd _ _ args =  do
    let [file] = args
    void $ processFile True file


evalCmd :: MainOptions -> EvalOptions -> [String] -> IO ()
evalCmd _ (EvalOptions optShow) args =  do
    let [file] = args
    program <- processFile optShow file

    when optShow $ putStr "-> "
    print $ DBT.Eval.eval program


runCmd :: MainOptions -> RunOptions -> [String] -> IO ()
runCmd _ (RunOptions optTrace) args = do
    let [file] = args
    instructions <- read <$> getFileContents file
    print $ ASM.eval optTrace (ASM.Code instructions)


main :: IO ()
main =
    runSubcommand
      [ subcommand "compile" compileCmd
      , subcommand "type" typeCmd
      , subcommand "eval" evalCmd
      , subcommand "run" runCmd
      ]
    `catch` (\(err::ErrorCall) -> putStrLn $ "Error: " ++ show err)
