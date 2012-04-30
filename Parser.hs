module Parser where

import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec
import Numeric (readHex, readOct, readFloat)
import Control.Monad.Error

import Types
import Num

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many $ choice [ try (string "\\n")
                       , try (string "\\r")
                       , try (string "\\t")
                       , try (string "\\\\")
                       ] <|> (liftM (:[]) $ noneOf "\"")
    --x <- many $ (noneOf "\"")
    char '"'
    return $ String $ concat x

parseChar :: Parser LispVal
parseChar = do -- TODO: #\space
  string "#\\"
  c <- anyChar
  return $ Character c

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of
                 "#t" -> Bool True
                 "#f" -> Bool False
                 _ -> Atom atom


-- Numeric types ----------------------------------------

parseBin :: Parser LispVal
parseBin = do
  string "#b"
  i <- many1 $ oneOf "01"
  return $ Number $ fromIntegral $ toInt i 2

parseOct :: Parser LispVal
parseOct = do
  string "#o"
  i <- many1 $ oneOf "01234576"
  return $ Number $ fromIntegral $ toInt i 8

parseDec :: Parser LispVal
parseDec = liftM (Number . fromIntegral . (flip toInt 10)) (many1 digit)

parseHex :: Parser LispVal
parseHex = do
  string "#x"
  i <- many1 $ oneOf "0123456789abcdef"
  return $ Number $ fromIntegral $ toInt i 16

parseNumber :: Parser LispVal
parseNumber =
      try (do i <- many1 $ oneOf "0123456789"
              char '.'
              f <- many1 $ oneOf "0123456789"
              return $ (Float . fst . (!! 0) . readFloat) (i ++ "." ++ f))
  <|> try parseHex
  <|> try parseOct
  <|> try parseBin
  <|> try parseDec

-- ------------------------------------------------------

parseList :: Parser LispVal
parseList = liftM List $ parseExpr `sepBy` spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- parseExpr `endBy` spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]


parseExpr :: Parser LispVal
parseExpr = parseNumber
        <|> parseString
        <|> parseAtom
        <|> parseChar
        <|> parseQuoted
        <|> do char '('
               x <- (try parseList) <|> parseDottedList
               char ')'
               return x

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> throwError $ Parser err
    Right val -> return val