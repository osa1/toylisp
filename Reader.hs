{-# LANGUAGE NoMonomorphismRestriction #-}
module Reader where

import Prelude hiding (readList)
import Control.Monad.ST
import Control.Monad.State
import qualified Text.Parsec as P
import qualified Text.ParserCombinators.Parsec as PC
import qualified Data.Map as M
import Data.List (intercalate)

import Types
{-import RParser-}

data ReadTable = ReadTable (M.Map Char (Reader LispVal))

instance Show ReadTable where
    show (ReadTable m) = "ReadTable [" ++ intercalate "," (map (:[]) $ M.keys m) ++ "]"

type ReaderS = (ReadTable,ReadTable)
type Reader = P.Parsec String ReaderS

runReader = P.runP

data ReadMacroChar = MacroChar Char | DMacroChar Char

readMacroChar :: Reader ReadMacroChar
readMacroChar = (liftM MacroChar) $ P.oneOf "\";\'@^`~()[]{}|\\%#"

dispatchMacroChar :: Reader ReadMacroChar
dispatchMacroChar = (liftM DMacroChar) $ P.char '#' >> P.oneOf "\";\'@^`~()[]{}|\\%#"

readExpr :: Reader LispVal
readExpr = do
    dc <- P.choice [ P.try $ dispatchMacroChar >> readMacroChar
                   , readMacroChar
                   ]
    ((ReadTable rt),(ReadTable drt)) <- P.getState
    let rf = 
         case dc of
           (MacroChar c)  -> M.lookup c rt
           (DMacroChar c) -> M.lookup c drt
    case rf of
        Nothing -> fail "??" -- TODO fail msg
        Just rf -> rf

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
