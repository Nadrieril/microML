module Parse.Program where

import Text.ParserCombinators.Parsec

import Parse.AST hiding (Infix)
import Parse.ADT
import Parse.Expr
import Parse.Token


program :: Parser Program
program = (,) <$> many adt <*> expr

parseML :: String -> Either ParseError Program
parseML = parse (whiteSpace >> program) ""
