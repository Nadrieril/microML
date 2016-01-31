{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module AST.Parse (parseML) where

import Data.Function (on)
import Data.Maybe (isJust)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import AST.Expr hiding (Infix, expr)
import qualified AST.Expr as AST

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

ident      = Name <$> Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis
natural    = Token.natural    lexer -- parses an natural
whiteSpace = Token.whiteSpace lexer -- parses whitespace


operators :: forall st. [[Operator Char st (Expr Name)]]
operators = [ [neg]
            , [mul, div]
            , [add, sub]
            , [and]
            , [or]
            , [eq] ]
    where
        f c v = reservedOp c >> return v
        inf o = AST.Infix (Name o) `on` untyped
        neg = Prefix (f "-" (inf "-" (Const $ I 0)))
        g o = Infix (f o (inf o)) AssocLeft
        mul = g "*"
        div = g "/"
        add = g "+"
        sub = g "-"
        or  = g "or"
        and = g "and"
        eq =  g "=="


letin :: Parser (Expr Name)
letin = do
    reserved "let"
    b <- optionMaybe $ reserved "rec"
    x <- ident
    reservedOp "="
    v <- expr
    reserved "in"
    e <- expr
    return $ (if isJust b then LetR else Let) x v e

ifthenelse :: Parser (Expr Name)
ifthenelse = do
    reserved "if"
    b <- expr
    reserved "then"
    e1 <- expr
    reserved "else"
    e2 <- expr
    return $ If b e1 e2

lambda :: Parser (Expr Name)
lambda = do
    reserved "fun"
    x <- ident
    reservedOp "->"
    e <- expr
    return $ Fun x e

boolean :: Parser (Expr Name)
boolean = (reserved "true" >> return (Const $ B True))
      <|> (reserved "false" >> return (Const $ B False))

atom :: Parser (Expr Name)
atom =  (Wrap <$> parens expr)
    <|> letin
    <|> ifthenelse
    <|> lambda
    <|> boolean
    <|> (Const . I) <$> natural
    <|> Var <$> ident
    <?> "atom"

funAp :: Parser (Expr Name)
funAp = foldl1 (Ap `on` untyped) <$> many1 atom

untyped :: Expr a -> TExpr a
untyped = LFixP Nothing

typed :: Parser (Expr Name) -> Parser (TExpr Name)
typed p = untyped <$> p

expr :: Parser (TExpr Name)
expr = typed (buildExpressionParser operators funAp
            <?> "expression")


parseML :: String -> Either ParseError (TExpr Name)
parseML = parse (whiteSpace >> expr) "(unknown)"
