module Parser where

import Types

import Text.ParserCombinators.Parsec
import Control.Applicative ((<*))
import Data.Set hiding (map)

wrapSpaces parser = spaces >> (parser <* spaces)

ident :: Parser (Expr Ident)
ident = fmap Ident $ many1 (oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ "-_/\\*+?")

lambda :: Parser (Expr Lambda)
lambda = do
    char '('
    wrapSpaces $ string "lambda"
    char '('
    param <- ident
    char ')'
    ex <- expr char ')'
    return $ Lambda param ex

application :: Parser (Expr Application)
application = do
    char '('
    e1 <- expr
    e2 <- expr
    char ')'
    return $ Application e1 e2

anyExpr :: Parser (Expr a) -> Parser AnyExpr
anyExpr p = fmap AnyExpr p

expr :: Parser AnyExpr
expr = choice [anyExpr ident, anyExpr application, anyExpr lambda]
