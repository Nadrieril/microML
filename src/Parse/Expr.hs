module Parse.Expr where

import Data.Function (on)
import Data.Maybe (isJust)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

import Common.Expr hiding (expr)
import Common.Pattern
import qualified Parse.AST as AST
import Parse.AST hiding (Infix)
import Parse.Token
import Parse.Type


-------------------------------------------------------------------------------
--- Literals ---
boolean :: Parser UntypedExpr
boolean = fmap (Const . B) (
          (reserved "true" >> return True)
      <|> (reserved "false" >> return False)
      <?> "boolean")

integer :: Parser UntypedExpr
integer = (Const . I) <$> natural
    <?> "integer"


-------------------------------------------------------------------------------
--- Operators ---
operators :: forall st. [[Operator Char st UntypedExpr]]
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
pattrnOperators :: forall st. [[Operator Char st (Pattern Name)]]
pattrnOperators = [ [r ":"], [l ","] ]
    where
        f o = Infix (reservedOp o >> return (\x y -> Pattern o [x, y]))
        l o = f o AssocLeft
        r o = f o AssocRight

pattrnAtom :: Parser (Pattern Name)
pattrnAtom = try (parens pattrn)
       <|> Pattern <$> typeIdentOrOp <*> many pattrnAtom
       <|> PVar <$> ident
    <?> "pattern atom"

pattrn :: Parser (Pattern Name)
pattrn = buildExpressionParser pattrnOperators pattrnAtom
     <?> "pattern expression"

matchexpr :: Parser (Pattern Name, Expr)
matchexpr = (,) <$> pattrn <* reservedOp "->" <*> expr

matchwith :: Parser UntypedExpr
matchwith = do
    reserved "match"
    Match
        <$> expr
        <*  reserved "with"
        <*  optional (reservedOp "|")
        <*> sepBy matchexpr (reservedOp "|")
        <*  reserved "end"
    <?> "match expression"


-------------------------------------------------------------------------------
--- Expressions ---
variable :: Parser UntypedExpr
variable = Var <$> identOrOp
    <?> "variable"

letin :: Parser UntypedExpr
letin = do
    reserved "let"
    b <- optionMaybe $ reserved "rec"
    (if isJust b then LetR else Let)
        <$> identOrOp
        <*> many ident
        <* reservedOp "=" <*> expr
        <* reserved "in" <*> expr
    <?> "let"

ifthenelse :: Parser UntypedExpr
ifthenelse = If <$> (reserved "if" >> expr)
                <*> (reserved "then" >> expr)
                <*> (reserved "else" >> expr)
    <?> "if"

lambda :: Parser UntypedExpr
lambda = Fun <$> (reserved "fun" >> identOrOp)
             <*> (reservedOp "->" >> expr)
    <?> "lambda"

atom :: Parser UntypedExpr
atom =  try variable
    <|> (Wrap <$> parens expr)
    <|> letin
    <|> matchwith
    <|> ifthenelse
    <|> lambda
    <|> boolean
    <|> integer
    <?> "atom"

funAp :: Parser UntypedExpr
funAp = foldl1 (Ap `on` untyped) <$> many1 atom <?> "function application"

expr :: Parser Expr
expr = typed (buildExpressionParser operators funAp <?> "expression")
