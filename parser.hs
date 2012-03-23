module Parser where

import Text.ParserCombinators.Parsec hiding (token)

data Sexp = List [Sexp] | Atom String deriving (Show)

sexps :: Parser [Sexp]
sexps = sexp `sepBy` listSeparator

sexp :: Parser Sexp
sexp = atom <|> list

atom :: Parser Sexp
atom = do
    s <- many1 (letter <|> digit)
    return $ Atom s

list :: Parser Sexp
list = do
    char '('
    sexps <- (list <|> atom) `sepBy` listSeparator
    char ')'
    return $ List sexps

listSeparator :: Parser ()
listSeparator = skipMany space