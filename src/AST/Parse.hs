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
import Typed.Type

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
        , Token.reservedOpNames = ["::", "->", "=", "+", "-", "*", "/", "<", ">", "==" ]
    }

lexer = Token.makeTokenParser languageDef

ident      = Name <$> Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis
natural    = Token.natural    lexer -- parses an natural
whiteSpace = Token.whiteSpace lexer -- parses whitespace

-----------------------
untyped :: UntypedExpr a -> TypedExpr a
untyped = LFixP Nothing

typeIdent :: Parser TConst
-- typeIdent = do
--     c <- upper
--     cs <- many (alphaNum <|> oneOf "_'")
--     return (c:cs)
typeIdent = (string "Int" >> return TInt)
        <|> (string "Bool" >> return TBool)
    <?> "type identifier"

typeAtom :: Parser (Mono Name)
typeAtom = TVar <$> ident
       <|> TConst <$> typeIdent
    <?> "type atom"

typ :: Parser (Mono Name)
typ = foldl1 (:->) <$> typeAtom `sepBy1` reservedOp "->"
    <?> "type annotation"


typed :: Parser (UntypedExpr Name) -> Parser (TypedExpr Name)
typed p = flip LFixP <$> p <*> optionMaybe (reservedOp "::" >> typ)

-----------------------
operators :: forall st. [[Operator Char st (UntypedExpr Name)]]
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


boolean :: Parser (UntypedExpr Name)
boolean = fmap (Const . B) (
          (reserved "true" >> return True)
      <|> (reserved "false" >> return False)
      <?> "boolean")

integer :: Parser (UntypedExpr Name)
integer = (Const . I) <$> natural
    <?> "integer"

variable :: Parser (UntypedExpr Name)
variable = Var <$> ident
    <?> "variable"

letin :: Parser (UntypedExpr Name)
letin = do
    reserved "let"
    b <- optionMaybe $ reserved "rec"
    (if isJust b then LetR else Let)
        <$> ident
        <*> (reservedOp "=" >> expr)
        <*> (reserved "in" >> expr)
    <?> "let"

ifthenelse :: Parser (UntypedExpr Name)
ifthenelse = If <$> (reserved "if" >> expr)
                <*> (reserved "then" >> expr)
                <*> (reserved "else" >> expr)
    <?> "if"

lambda :: Parser (UntypedExpr Name)
lambda = Fun <$> (reserved "fun" >> ident)
             <*> (reservedOp "->" >> expr)
    <?> "lambda"


atom :: Parser (UntypedExpr Name)
atom =  (Wrap <$> parens expr)
    <|> letin
    <|> ifthenelse
    <|> lambda
    <|> boolean
    <|> integer
    <|> variable
    <?> "atom"

funAp :: Parser (UntypedExpr Name)
funAp = foldl1 (Ap `on` untyped) <$> many1 atom <?> "function application"

expr :: Parser Expr
expr = typed (buildExpressionParser operators funAp <?> "expression")


parseML :: String -> Either ParseError Expr
parseML = parse (whiteSpace >> expr) "Parse error"
