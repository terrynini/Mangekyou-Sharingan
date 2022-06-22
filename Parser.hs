module Parser where

-- different ways to import https://wiki.haskell.org/Import
import Lexer
import Syntax
import Text.Parsec
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token
import Text.Parsec.Expr (buildExpressionParser)

-- original definition: binary s f accos = Ex.Infix (reservedOp s >> return (BinOp f)) accos
-- the accos can be omitted due to currying
binary s f = Ex.Infix (reservedOp s >> return (BinOp f))

-- https://hackage.haskell.org/package/parsec-3.1.14.0/docs/Text-Parsec-Expr.html#v:buildExpressionParser
table = [[binary "*" Times Ex.AssocLeft, binary "/" Divide Ex.AssocLeft],
         [binary "+" Plus Ex.AssocLeft, binary "-" Minus Ex.AssocLeft]]

expr :: Parser Expr
expr = buildExpressionParser table factor

{-
floating :: Parser Expr
floating = do
    n <- float
    return $ Float n
-}
-- <$> is an infix synonym of fmap : https://hoogle.haskell.org/?hoogle=%3C%24%3E
floating :: Parser Expr
floating = do Float <$> float

-- explain of <|> : http://book.realworldhaskell.org/read/using-parsec.html
factor :: Parser Expr
factor = try floating


-- defn :: Parser Expr
-- defn = expr

parseExpr :: String -> Either ParseError Expr
parseExpr = parse expr "<stdin>"
