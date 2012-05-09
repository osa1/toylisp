{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-hi-shadowing #-}

module RParser where

import Control.Monad
import Text.ParserCombinators.Parsec as P
import Control.Applicative ((<*))

import Types

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

string :: Parser LispVal
string = (liftM String) $ many $ noneOf "\"" <* P.char '"'

char :: Parser LispVal
char = (liftM Character) $ anyChar >> P.char '\''

atom :: Parser LispVal
atom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = [first] ++ rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

-- ----------------------------------------
-- Numeric types
-- ----------------------------------------

int :: Parser LispVal
{-int = many1 digit >>= (liftM $ Number . read)-}
int = do
    nums <- many1 digit
    return $ Number (read nums)

number :: Parser LispVal
number = choice [int] -- TODO

-- ----------------------------------------
-- Expressions
-- ----------------------------------------

-- TODO: I'm not sure about necessity of dotted pairs

readMacroChar :: Parser Char
readMacroChar = oneOf "\";\'@^`~()[]{}|\\%#"

dispatchMacroChar :: Parser Char
dispatchMacroChar = P.char '#'

list :: Parser LispVal
list = liftM List $ expr `sepBy` spaces
