{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Parse (parseML) where

import Control.Monad (when)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import IR.AST

-----------------------------------------------------------------------------
languageDef =
    emptyDef
        { Token.commentStart    = "(*"
        , Token.commentEnd      = "*)"
        , Token.commentLine     = "//"
        , Token.reservedNames   = [ "if" , "then" , "else"
                                  , "let", "rec", "in", "fun"
                                  , "true" , "false"
                                  , "and", "or", "not"
                                  ]
        , Token.reservedOpNames = ["=", "+", "-", "*", "/", "<", ">", "==", "->" ]
    }

lexer = Token.makeTokenParser languageDef

ident      = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis
natural    = Token.natural    lexer -- parses an natural
whiteSpace = Token.whiteSpace lexer -- parses whitespace


operators :: forall st. [[Operator Char st (Expr String)]]
operators = [ [neg]
            , [mul, div]
            , [add, sub]
            , [and]
            , [or]
            , [eq] ]
    where
        f c v = reservedOp c >> return v
        neg = Prefix (f "-"   Neg                )
        mul = Infix  (f "*"   (ABinary Multiply) ) AssocLeft
        div = Infix  (f "/"   (ABinary Divide)   ) AssocLeft
        add = Infix  (f "+"   (ABinary Add)      ) AssocLeft
        sub = Infix  (f "-"   (ABinary Subtract) ) AssocLeft
        or  = Infix  (f "or"  (ABinary Or)       ) AssocLeft
        and = Infix  (f "and" (ABinary And)      ) AssocLeft
        eq =  Infix  (f "=="  (ABinary Eq)       ) AssocLeft


letin :: Bool -> Parser (Expr String)
letin b = do
    reserved "let"
    when b $ reserved "rec"
    x <- ident
    reservedOp "="
    v <- expr
    reserved "in"
    e <- expr
    return $ (if b then LetR else Let) x v e

letnonrec :: Parser (Expr String)
letnonrec = letin False
letrec :: Parser (Expr String)
letrec = letin True


ifthenelse :: Parser (Expr String)
ifthenelse = do
    reserved "if"
    b <- expr
    reserved "then"
    e1 <- expr
    reserved "else"
    e2 <- expr
    return $ If b e1 e2

lambda :: Parser (Expr String)
lambda = do
    reserved "fun"
    x <- ident
    reservedOp "->"
    e <- expr
    return $ Fun x e

boolean :: Parser (Expr String)
boolean = (reserved "true" >> return (BoolConst True))
      <|> (reserved "false" >> return (BoolConst False))

atom :: Parser (Expr String)
atom =  parens expr
    <|> try letnonrec
    <|> letrec
    <|> ifthenelse
    <|> lambda
    <|> boolean
    <|> IntConst <$> natural
    <|> Var <$> ident
    <?> "atom"

funAp :: Parser (Expr String)
funAp = foldl1 Ap <$> many1 atom

expr :: Parser (Expr String)
expr = buildExpressionParser operators funAp
            <?> "expression"


parseML :: String -> Either ParseError (Expr String)
parseML = parse (whiteSpace >> expr) "(unknown)"
