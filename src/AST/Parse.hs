{-# LANGUAGE FlexibleContexts, RankNTypes #-}
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
import Common.ADT
import Common.Pattern
import Common.Type

-------------------------------------------------------------------------------
--- Lexer / Parser ---
languageDef =
    emptyDef
        { Token.commentStart    = "(*"
        , Token.commentEnd      = "*)"
        , Token.commentLine     = "//"
        , Token.reservedNames   = [ "if" , "then" , "else"
                                  , "let", "rec", "in", "fun"
                                  , "data", "true" , "false"
                                  , "match", "with", "end"
                                  , "Int", "Bool"
                                  ]
        , Token.reservedOpNames = ["::", "->", "=", "|" ]
        , Token.opStart = Token.opLetter languageDef
        , Token.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~,"
    }

lexer = Token.makeTokenParser languageDef

ident      = Token.identifier lexer
operator   = Token.operator lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens     lexer
natural    = Token.natural    lexer
whiteSpace = Token.whiteSpace lexer

isOperator :: Name -> Bool
isOperator = isRight . parse operator ""

identOrOp :: Parser Name
identOrOp = ident <|> parens operator

-------------------------------------------------------------------------------
--- Literals ---
boolean :: Parser (UntypedExpr Name)
boolean = fmap (Const . B) (
          (reserved "true" >> return True)
      <|> (reserved "false" >> return False)
      <?> "boolean")

integer :: Parser (UntypedExpr Name)
integer = (Const . I) <$> natural
    <?> "integer"

-------------------------------------------------------------------------------
--- Types ---
untyped :: UntypedExpr a -> TypedExpr a
untyped = LFixP Nothing

typeIdent :: Parser Name
typeIdent = do
        c <- upper
        cs <- many (alphaNum <|> oneOf "_'")
        whiteSpace
        return (c:cs)
    <?> "type identifier"

typeAtom :: Parser (Mono Name)
typeAtom = parens typ
       <|> TConst <$> (reserved "Int" *> return TInt)
       <|> TConst <$> (reserved "Bool" *> return TBool)
       <|> TProduct <$> typeIdent <*> many typeAtom
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

-------------------------------------------------------------------------------
--- ADTs ---
constructor :: Parser (Constructor Name)
constructor = Constructor <$> typeIdent <*> many typeAtom

adt :: Parser (ADT Name)
adt = do
    reserved "data"
    name <- typeIdent
    params <- many ident
    reservedOp "="
    constructors <- sepBy constructor (reservedOp "|")
    reserved "in"
    return $ ADT name params constructors Nothing
    <?> "ADT"

-------------------------------------------------------------------------------
--- Operators ---
operators :: forall st. [[Operator Char st (UntypedExpr Name)]]
operators = [ [neg]
            , [l "*", l "/"]
            , [l "+", l "-"]
            , [l "=="]
            , [l "&&"]
            , [l "||"]
            , [anythingelse]
            , [r "$"]
            , [l ","]
            ]
    where
        f c v = reservedOp c >> return v
        neg = Prefix (f "-" (Negate . untyped))
        inf = Infix . fmap (\o -> AST.Infix o `on` untyped)
        l o = inf (f o o) AssocLeft
        r o = inf (f o o) AssocRight
        anythingelse = inf otherop AssocRight
        otherop = try $ do
            x <- operator
            if x `elem` ["$", "&&", "||", "==", "*", "/", "+", "-", "$", ","]
                then unexpected (show x)
                else return x

-------------------------------------------------------------------------------
--- Pattern-matching ---
pattrn :: Parser (Pattern Name, Expr)
pattrn = do
    pat <- Pattern <$> typeIdent <*> many ident
    reservedOp "->"
    e <- expr
    return (pat, e)

matchwith :: Parser (UntypedExpr Name)
matchwith = do
    reserved "match"
    e <- expr
    reserved "with"
    optional $ reservedOp "|"
    patterns <- sepBy pattrn (reservedOp "|")
    reserved "end"
    return $ Match e patterns
    <?> "match expression"


-------------------------------------------------------------------------------
--- Expressions ---
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
    <|> matchwith
    <|> ifthenelse
    <|> lambda
    <|> boolean
    <|> integer
    <?> "atom"

funAp :: Parser (UntypedExpr Name)
funAp = foldl1 (Ap `on` untyped) <$> many1 atom <?> "function application"

expr :: Parser Expr
expr = typed (buildExpressionParser operators funAp <?> "expression")

-------------------------------------------------------------------------------
--- Program ---
program :: Parser Program
program = (,) <$> many adt <*> expr


parseML :: String -> Either ParseError Program
parseML = parse (whiteSpace >> program) ""
