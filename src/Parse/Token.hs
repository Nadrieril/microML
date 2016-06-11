module Parse.Token where

import Data.Either (isRight)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Common.Expr hiding (expr)

-------------------------------------------------------------------------------
--- Tokens ---
languageDef =
    emptyDef
        { Token.commentStart    = "(*"
        , Token.commentEnd      = "*)"
        , Token.commentLine     = "//"
        , Token.reservedNames   = [ "if" , "then" , "else"
                                  , "let", "rec", "in", "fun"
                                  , "data", "true" , "false"
                                  , "match", "with", "end"
                                  ]
        , Token.reservedOpNames = ["::", "->", "=", "|" ]
        , Token.opStart = Token.opLetter languageDef
        , Token.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~,"
    }

lexer = Token.makeTokenParser languageDef

ident      = Token.identifier lexer
operator   = Token.operator   lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens     lexer
natural    = Token.natural    lexer
whiteSpace = Token.whiteSpace lexer

isOperator :: Name -> Bool
isOperator = isRight . parse operator ""

identOrOp :: Parser Name
identOrOp = ident <|> parens operator

typeIdent :: Parser Name
typeIdent = do
        c <- upper
        cs <- many (alphaNum <|> oneOf "_'")
        whiteSpace
        return (c:cs)
    <?> "type identifier"

typeIdentOrOp :: Parser Name
typeIdentOrOp = typeIdent <|> parens operator
