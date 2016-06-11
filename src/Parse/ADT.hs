module Parse.ADT where

import Text.ParserCombinators.Parsec

import Common.Expr hiding (expr)
import Common.ADT
import Parse.Token
import Parse.Type


constructor :: Parser (Constructor Name)
constructor = Constructor <$> typeIdentOrOp <*> many typeAtom

adt :: Parser (ADT Name)
adt = do
    reserved "data"
    name <- typeIdent
    params <- many ident
    reservedOp "="
    constructors <- sepBy constructor (reservedOp "|")
    reserved "in"
    return $ ADT name params constructors
    <?> "ADT"
