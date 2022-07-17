module Lexer where

import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (GenTokenParser (reservedOp))
import qualified Text.Parsec.Token as Token

-- define a lexer with custom token definition base on emptyDef
lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where
    reservedOpNames = ["+", "-", "*", "/"]
    reservedNames = ["def", "extern"]
    style =
      emptyDef
        { Token.commentLine = "#",
          Token.reservedOpNames = reservedOpNames,
          Token.reservedNames = reservedNames
        }

-- use different lexeme to define primitive tokens
integer :: Parser Integer
integer = Token.integer lexer

float :: Parser Double
float = Token.float lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

commaSep :: Parser a -> Parser [a]
commaSep = Token.commaSep lexer

semiSep :: Parser a -> Parser [a]
semiSep = Token.semiSep lexer

identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer