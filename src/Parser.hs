{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-hi-shadowing -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}
{-# LANGUAGE GADTs #-}
module Parser where

import Control.Monad
import Text.ParserCombinators.Parsec
import Numeric (readFloat)
import Control.Applicative ((<*))
import Control.Monad.Error (throwError)

import Types
import Num

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

parseString :: Parser TVal
parseString = do
    char '"'
    s <- many $ noneOf "\""
    char '"'
    return $ String s

parseChar :: Parser TVal
parseChar = do -- TODO: #\space
  string "#\\"
  c <- anyChar
  return $ Char c

parseBool :: Parser TVal
parseBool = do
  char '#'
  c <- char 't' <|> char 'f'
  case c of
    't' -> return $ Bool True
    'f' -> return $ Bool False
    _ -> unexpected [c]

-- Numeric types ----------------------------------------

parseBin :: Parser TVal
parseBin = do
  string "#b"
  i <- many1 $ oneOf "01"
  return $ Int $ fromIntegral $ toInt i 2

parseOct :: Parser TVal
parseOct = do
  string "#o"
  i <- many1 $ oneOf "01234576"
  return $ Int $ fromIntegral $ toInt i 8

parseDec :: Parser TVal
parseDec = liftM (Int . fromIntegral . flip toInt 10) (many1 digit)

parseHex :: Parser TVal
parseHex = do
  string "#x"
  i <- many1 $ oneOf "0123456789abcdef"
  return $ Int $ fromIntegral $ toInt i 16

parseNumber :: Parser TVal
parseNumber =
      try (do i <- many1 $ oneOf "0123456789"
              char '.'
              f <- many1 $ oneOf "0123456789"
              return $ (Float . fst . (!! 0) . readFloat) (i ++ "." ++ f)) <* spaces
  <|> try parseHex <* spaces
  <|> try parseOct <* spaces
  <|> try parseBin <* spaces
  <|> try parseDec <* spaces

-- Helpers ------------------------------------------------

spChar :: Char -> Parser Char
spChar c = char c <* spaces

spString :: String -> Parser String
spString s = string s <* spaces

-- Expressions --------------------------------------------

parseSymbol :: Parser (Expr Symbol)
parseSymbol = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  spaces
  return $ Symbol (first : rest)

parseArgList :: Parser [Expr Symbol]
parseArgList = (spChar '(' >> many spacedArg) <* spChar ')'
  where spacedArg :: Parser (Expr Symbol)
        spacedArg = parseSymbol <* spaces

parseLambda :: Parser (Expr Lambda)
parseLambda = do
    spChar '('
    spString "lambda"
    args <- parseArgList
    body <- many1 parseAnyExpr
    spChar ')'
    return $ Lambda args body

parseLambdaOrFun :: Parser (Either (Expr Symbol) (Expr Lambda))
parseLambdaOrFun = liftM Right parseLambda <|> liftM Left parseSymbol

-- TODO: this is wrong, too. Try this ((higher-order-fun some-other-fun) param1 param2)
--parseApplication :: Parser (Expr Application)
--parseApplication = do
--    spChar '('
--    fun <- parseLambdaOrFun
--    params <- many parseAnyExpr
--    spChar ')'
--    return $ Application fun params

parseApplication :: Parser (Expr Application)
parseApplication = do
  spChar '('
  fun <- parseAnyExpr
  params <- many parseAnyExpr
  spChar ')'
  return $ Application fun params

parseIf :: Parser (Expr If)
parseIf = do
    spChar '('
    spString "if"
    pred <- parseAnyExpr
    ifE <- parseAnyExpr
    thenE <- parseAnyExpr
    spChar ')'
    return $ If pred ifE thenE

parseDelimitedList :: Char -> Char -> Parser (Expr List)
parseDelimitedList o c = do
    spChar o
    lst <- many parseAnyExpr
    spChar c
    return $ List lst

parseList :: Parser (Expr List)
parseList = do
    spChar '('
    spString "list"
    lst <- many parseAnyExpr
    spChar ')'
    return $ List lst

parseCallCC :: Parser (Expr CallCC)
parseCallCC = do
    spChar '('
    spString "call/cc"
    fun <- parseLambdaOrFun
    spChar ')'
    return $ CallCC fun

parseFexprDefine :: Parser (Expr Define)
parseFexprDefine = do
  spString "$("
  spString "deff"
  name <- parseSymbol
  params <- parseArgList
  body <- many1 parseAnyExpr
  spChar ')'
  return $ Define name (AnyExpr $ Fexpr params body)

parseFexpr :: Parser (Expr Fexpr)
parseFexpr = do
  spString "$("
  spString "deff"
  params <- parseArgList
  body <- many1 parseAnyExpr
  spChar ')'
  return $ Fexpr params body

parseDef = try parseDefine <|> try parseDefun

parseDefine :: Parser (Expr Define)
parseDefine = do
  spChar '('
  spString "define"
  name <- parseSymbol
  def <- parseAnyExpr
  spChar ')'
  return $ Define name def

parseDefun :: Parser (Expr Define)
parseDefun = do
  spChar '('
  spString "defun"
  name <- parseSymbol
  params <- parseArgList
  body <- many1 parseAnyExpr
  spChar ')'
  return $ Define name (AnyExpr $ Lambda params body)


parseSet :: Parser (Expr Set)
parseSet = do
  spChar '('
  spString "set!"
  name <- parseSymbol
  def <- parseAnyExpr
  spChar ')'
  return $ Set name def

parseQList :: Parser (Expr List)
parseQList = do
  spString "'("
  lst <- many parseAnyExpr
  spChar ')'
  return $ List lst


anyExpr = liftM AnyExpr
val a = anyExpr $ liftM Val a

parseDispatch :: Parser (Expr List)
parseDispatch = undefined


consSymbol :: Expr List -> String -> Parser (Expr List)
consSymbol (List lst) s = return $ List ((AnyExpr $ Symbol s) : lst)

mapMacro :: Expr List -> Parser (Expr List)
mapMacro = flip consSymbol "make-map"

vectorMacro :: Expr List -> Parser (Expr List)
vectorMacro = flip consSymbol "make-vector"

parseAnyExpr :: Parser AnyExpr
parseAnyExpr = do
    spaces
    choice  [ val $ try parseBool
            , val $ try parseChar
            , val $ try parseNumber
            , val $ try parseString
            , anyExpr $ try parseFexprDefine
            , anyExpr $ try parseFexpr
            , anyExpr $ try parseSymbol
            , anyExpr $ try parseLambda
            , anyExpr $ try parseIf
            , anyExpr $ try parseCallCC
            , anyExpr $ try parseDef
            , anyExpr $ try parseSet
            , anyExpr $ try parseList
            , anyExpr $ try parseApplication
            --, anyExpr $ try (parseDelimitedList '(' ')')
            , anyExpr $ try parseQList
            , anyExpr $ try (parseDelimitedList '[' ']') >>= vectorMacro
            , anyExpr $ try (parseDelimitedList '{' '}') >>= mapMacro
            ]

readOrThrow :: Parser a -> String -> IOThrowsError a
readOrThrow parser input = case parse parser "expression" input of
  Left err -> throwError $ Parser err
  Right val -> return val

readExpr :: String -> IOThrowsError AnyExpr
readExpr = readOrThrow parseAnyExpr

readAsSyntax :: String -> IOThrowsError TVal
readAsSyntax str = case parse parseAnyExpr "syntax" str of
  Left err -> throwError $ Parser err
  Right val -> return $ Syntax val
