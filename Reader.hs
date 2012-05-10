{-# LANGUAGE NoMonomorphismRestriction #-}
module Reader where

import Prelude hiding (readList)
import Control.Monad.ST
import Control.Monad.State
import qualified Text.Parsec as P
import qualified Text.ParserCombinators.Parsec as PC
import qualified Data.Map as M
import Data.List (intercalate)
import Control.Applicative ((<*))

import Types
{-import RParser-}

-- XXX this data type is recursive, Reader type has ReaderS,
--     and ReaderS has ReadTable again.
data ReadTable = ReadTable (M.Map Char (Reader LispVal))

instance Show ReadTable where
    show (ReadTable m) = "ReadTable [" ++ intercalate "," (map (:[]) $ M.keys m) ++ "]"

type ReaderS = (ReadTable,ReadTable)
type Reader = P.Parsec String ReaderS

runReader = P.runP

data ReadMacroChar = MacroChar Char | DMacroChar Char

readMacroChar :: Reader ReadMacroChar
readMacroChar = (liftM MacroChar) $ P.oneOf "\";\'@^`~([{|\\%#"

dispatchMacroChar :: Reader ReadMacroChar
dispatchMacroChar = (liftM DMacroChar) $ P.char '#' >> P.oneOf "\";\'@^`~([{|\\%#"

readExpr :: Reader LispVal
readExpr = do
    dc <- P.choice [
                     P.try $ dispatchMacroChar >> readMacroChar
                   , readMacroChar
                   ]
    ((ReadTable rt),(ReadTable drt)) <- P.getState
    let rf = 
         case dc of
           (MacroChar c)  -> M.lookup c rt
           (DMacroChar c) -> M.lookup c drt
    maybe (fail "??") id rf -- TODO fail msg

readExprs :: Reader [LispVal]
readExprs = readExpr `P.endBy` P.spaces

readSymbol :: Reader LispVal
readSymbol = do
    let cs = P.oneOf "+-*/_"
    first <- P.choice [P.letter,cs]
    rest <- P.many $ P.choice [P.letter,P.digit,cs]
    let atom = [first] ++ rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

readString :: Reader LispVal
readString = (liftM String) $ (P.many $ P.noneOf "\"") <* P.char '"'

readChar :: Reader LispVal
readChar = (liftM Character) $ P.anyChar <* P.char '\''

-- TODO other numeric types
readNumber :: Reader LispVal
readNumber = (liftM $ Number . read) $ P.many1 P.digit

readDelimitedList :: Char -> Reader LispVal
readDelimitedList c = (liftM List) $ readExprs <* P.char c

readMacroTable = ReadTable $ M.fromList
                    [ ('\'', readChar)
                    , ('"',  readString)
                    , ('(',  readDelimitedList ')')
                    ]

main :: IO ()
main = do
    s <- getContents
    let p = P.runParser readExprs (readMacroTable,readMacroTable) "lisp" s
    print p

-- readExpr :: ReadTable -> Parser LispVal
-- readExpr (ReadTable m) = do
--     c <- P.anyChar
--     case M.lookup c m of
--         Nothing -> 
-- 
-- initReadTable = ReadTable $
--     M.fromList [ ('(',  readList)
--                , ('"',  readString)
--                , ('\'', readQuote)
--                ]
-- 
-- initDReadTable = ReadTable $
--     M.fromList [ ('\\', readChar)
--                ]
-- 
-- tablesToParser :: ReadTable -> ReadTable -> P.Parser LispVal
-- tablesToParser (ReadTable m1) (ReadTable m2) = 
--     P.choice $ map (\(c,f) -> P.char c >> f) (M.toList m1)
--             ++ map (\(c,f) -> P.char c >> f) (M.toList m2)
-- 
-- initReaderS :: ReaderS
-- initReaderS = let rt  = initReadTable
--                   drt = initDReadTable
--               in (tablesToParser rt drt,rt,drt)
-- 
-- main :: IO ()
-- main = do
--     s <- getContents
--     let (parser,_,_) = initReaderS
--     r <- (P.parseTest parser s)
--     print r
