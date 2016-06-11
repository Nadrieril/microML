module Parse.Type where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

import Common.Expr hiding (expr)
import Common.Type
import Parse.AST hiding (Infix)
import Parse.Token


untyped :: UntypedExpr-> TypedExpr
untyped = LFixP Nothing

typeAtom :: Parser (Mono Name)
typeAtom = parens typ
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

typed :: Parser UntypedExpr -> Parser TypedExpr
typed p = flip LFixP <$> p <*> optionMaybe (reservedOp "::" >> typ)
