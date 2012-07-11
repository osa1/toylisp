{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-hi-shadowing #-}

module Parser where

import Control.Monad
import Text.ParserCombinators.Parsec
import Numeric (readFloat)
import Control.Monad.Error
import Control.Applicative ((<*))

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
  return $ case c of
             't' -> Bool True
             'f' -> Bool False

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
parseLambdaOrFun = (liftM Right parseLambda <|> liftM Left parseSymbol)

parseApplication :: Parser (Expr Application)
parseApplication = do
    spChar '('
    fun <- parseLambdaOrFun
    params <- many parseAnyExpr
    spChar ')'
    return $ Application fun params

parseIf :: Parser (Expr If)
parseIf = do
    spChar '('
    pred <- parseAnyExpr
    ifE <- parseAnyExpr
    thenE <- parseAnyExpr
    spChar ')'
    return $ If pred ifE thenE

parseEval :: Parser (Expr EvalExp)
parseEval = do
    spChar '('
    spString "eval"
    expr <- parseAnyExpr
    spChar ')'
    return $ EvalExp expr

parseCallCC :: Parser (Expr CallCC)
parseCallCC = do
    spChar '('
    spString "call/cc"
    fun <- parseLambdaOrFun
    body <- many1 parseAnyExpr
    spChar ')'
    return $ CallCC fun body

anyExpr = liftM AnyExpr
val a = anyExpr $ liftM Val a

parseAnyExpr :: Parser AnyExpr
parseAnyExpr = choice  [ val $ try parseBool
                       , val $ try parseNumber
                       , val $ try parseString
                       , anyExpr $ try parseSymbol
                       , anyExpr $ try parseLambda
                       , anyExpr $ try parseApplication
                       , anyExpr $ try parseIf
                       , anyExpr $ try parseEval
                       , anyExpr $ try parseCallCC
                       ]




--parseList :: Parser LispVal
--parseList = liftM List $ parseExpr `sepBy` spaces

--parseDottedList :: Parser LispVal
--parseDottedList = do
--  head <- parseExpr `endBy` spaces
--  tail <- char '.' >> spaces >> parseExpr
--  --tail <- char '.' >> spaces >> parseExpr
--  return $ DottedList head tail

--parseQuoted :: Parser LispVal
--parseQuoted = do
--  char '\''
--  x <- parseExpr
--  return $ List [Atom "quote", x]

--parseDelimitedList :: Char -> Char -> Parser LispVal
--parseDelimitedList open close = do
--    char open
--    x <- try parseList <|> parseDottedList
--    char close
--    return x

--makeVector :: LispVal -> LispVal
--makeVector (List lst) = List $ Atom "vector" : lst

--parseExpr :: Parser LispVal
--parseExpr = parseNumber
--        <|> parseString
--        <|> parseAtom
--        <|> parseChar
--        <|> parseQuoted
--        -- <|> parseQuasiquotation
--        -- <|> parseDispatchMacro
--        <|> liftM makeVector (parseDelimitedList '[' ']')
--        <|> parseDelimitedList '(' ')'

--readOrThrow :: Parser a -> String -> IOThrowsError a
--readOrThrow parser input = case parse parser "lisp" input of
--    Left err -> throwError $ Parser err
--    Right val -> return val

--readExpr :: String -> IOThrowsError LispVal
--readExpr = readOrThrow parseExpr

--readExprList :: String -> IOThrowsError [LispVal]
--readExprList = readOrThrow (endBy parseExpr spaces)

