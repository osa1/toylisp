module Parser where

import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec
import Numeric (readHex, readOct)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Character Char
             | Bool Bool
    deriving (Show)

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

parseNumber :: Parser LispVal
--parseNumber = liftM (Number . read) $ many1 digit
parseNumber =
    choice [ try $ (string "#b") >> liftM (Number . read) (many1 $ oneOf "01") -- TODO
           , try $ (string "#o") >> liftM (Number . fst . (!! 0) . readOct) (many1 $ oneOf "01234567")
           , try $ (string "#x") >> liftM (Number . fst . (!! 0) . readHex) (many1 $ oneOf "0123456789abcdef")
           , liftM (Number . read) $ many1 digit
           ]




parseExpr :: Parser LispVal
parseExpr = try parseNumber <|> parseString <|> parseAtom <|> parseChar

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"
