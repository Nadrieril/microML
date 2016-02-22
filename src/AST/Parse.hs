{-# LANGUAGE FlexibleContexts, RankNTypes, OverloadedStrings #-}
module AST.Parse (isOperator, parseML) where

import Data.Function (on)
import Data.Maybe (isJust)
import Data.Either (isRight)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Common.Expr hiding (expr)
import AST.Expr hiding (Infix)
import qualified AST.Expr as AST
import Common.Type

-----------------------------------------------------------------------------
languageDef =
    emptyDef
        { Token.commentStart    = "(*"
        , Token.commentEnd      = "*)"
        , Token.commentLine     = "//"
        , Token.reservedNames   = [ "if" , "then" , "else"
                                  , "let", "rec", "in", "fun"
                                  , "true" , "false"
                                  ]
        , Token.reservedOpNames = ["::", "->", "=" ]
        , Token.opStart = Token.opLetter languageDef
        , Token.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~,"
    }

lexer = Token.makeTokenParser languageDef

ident      = Name <$> Token.identifier lexer -- parses an identifier
operator   = Name <$> Token.operator lexer -- parses an operator
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis
natural    = Token.natural    lexer -- parses an natural
whiteSpace = Token.whiteSpace lexer -- parses whitespace

-----------------------
isOperator :: Name -> Bool
isOperator (Name n) = isRight . parse operator ""$ n

-----------------------
untyped :: UntypedExpr a -> TypedExpr a
untyped = LFixP Nothing

typeIdent :: Parser TConst
typeIdent = do
    n <- do
        c <- upper
        cs <- many (alphaNum <|> oneOf "_'")
        whiteSpace
        return (c:cs)
    case n of
        "Int" -> return TInt
        "Bool" -> return TBool
        _ -> error "unknown type"
    <?> "type identifier"

typeAtom :: Parser (Mono Name)
typeAtom = parens typ
       <|> TConst <$> typeIdent
       <|> TVar <$> ident
    <?> "type atom"

typeOperators :: forall st. [[Operator Char st (Mono Name)]]
typeOperators = [ [tuple], [fun] ]
    where
        fun = Infix (reservedOp "->" >> return (:->)) AssocRight
        tuple = Infix (reservedOp "," >> return TTuple) AssocLeft

typ :: Parser (Mono Name)
typ = buildExpressionParser typeOperators typeAtom
    <?> "type annotation"

typed :: Parser (UntypedExpr Name) -> Parser (TypedExpr Name)
typed p = flip LFixP <$> p <*> optionMaybe (reservedOp "::" >> typ)

-----------------------
operators :: forall st. [[Operator Char st (UntypedExpr Name)]]
operators = [ [l "&&"]
            , [l "||"]
            , [l "=="]
            , [neg]
            , [l "*", l "/"]
            , [l "+", l "-"]
            , [anythingelse]
            , [r "$"]
            , [l ","]
            ]
    where
        f c v = reservedOp c >> return v
        inf o = AST.Infix o `on` untyped
        neg = Prefix (f "-" (inf "-" (Const $ I 0)))
        l o = Infix (f o (inf $ Name o)) AssocLeft
        r o = Infix (f o (inf $ Name o)) AssocRight
        anythingelse = Infix (inf <$> otherop) AssocRight
        otherop = try $ do
            x <- operator
            if x `elem` ["$", "&&", "||", "==", "*", "/", "+", "-", "$", ","]
                then unexpected (show x)
                else return x


identOrOp :: Parser Name
identOrOp = ident <|> parens operator

boolean :: Parser (UntypedExpr Name)
boolean = fmap (Const . B) (
          (reserved "true" >> return True)
      <|> (reserved "false" >> return False)
      <?> "boolean")

integer :: Parser (UntypedExpr Name)
integer = (Const . I) <$> natural
    <?> "integer"

variable :: Parser (UntypedExpr Name)
variable = Var <$> identOrOp
    <?> "variable"

letin :: Parser (UntypedExpr Name)
letin = do
    reserved "let"
    b <- optionMaybe $ reserved "rec"
    (if isJust b then LetR else Let)
        <$> identOrOp
        <*> (reservedOp "=" >> expr)
        <*> (reserved "in" >> expr)
    <?> "let"

ifthenelse :: Parser (UntypedExpr Name)
ifthenelse = If <$> (reserved "if" >> expr)
                <*> (reserved "then" >> expr)
                <*> (reserved "else" >> expr)
    <?> "if"

lambda :: Parser (UntypedExpr Name)
lambda = Fun <$> (reserved "fun" >> identOrOp)
             <*> (reservedOp "->" >> expr)
    <?> "lambda"


atom :: Parser (UntypedExpr Name)
atom =  try variable
    <|> (Wrap <$> parens expr)
    <|> letin
    <|> ifthenelse
    <|> lambda
    <|> boolean
    <|> integer
    <?> "atom"

funAp :: Parser (UntypedExpr Name)
funAp = foldl1 (Ap `on` untyped) <$> many1 atom <?> "function application"

expr :: Parser Expr
expr = typed (buildExpressionParser operators funAp <?> "expression")


parseML :: String -> Either ParseError Expr
parseML = parse (whiteSpace >> expr) ""
