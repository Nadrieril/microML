{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Parse (parseML) where

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
                                  , "true" , "false"
                                  , "not" , "and" , "or"
                                  ]
        , Token.reservedOpNames = ["+", "-", "*", "/", "<", ">", "and", "or", "not" ]
    }

lexer = Token.makeTokenParser languageDef

ident      = Token.identifier lexer -- parses an identifier
-- reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis
natural    = Token.natural    lexer -- parses an natural
-- whiteSpace = Token.whiteSpace lexer -- parses whitespace


operators :: forall st. [[Operator Char st Expr]]
operators = [ [neg]
            , [mul, div]
            , [add, sub]
            , [and]
            , [or] ]
    where
        f c v = reservedOp c >> return v
        neg = Prefix (f "-"   Neg                )
        mul = Infix  (f "*"   (ABinary Multiply) ) AssocLeft
        div = Infix  (f "/"   (ABinary Divide)   ) AssocLeft
        add = Infix  (f "+"   (ABinary Add)      ) AssocLeft
        sub = Infix  (f "-"   (ABinary Subtract) ) AssocLeft
        or  = Infix  (f "or"  (ABinary Or)       ) AssocLeft
        and = Infix  (f "and" (ABinary And)      ) AssocLeft



atom :: Parser Expr
atom =  parens expr
    <|> IntConst <$> natural
    <|> Var <$> ident
    <?> "atom"

funAp :: Parser Expr
funAp = foldl1 Ap <$> many1 atom

expr :: Parser Expr
expr = buildExpressionParser operators funAp
            <?> "expression"


parseML :: String -> Either ParseError Expr
parseML = parse expr "(unknown)"
