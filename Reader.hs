module Reader where

import Control.Monad.ST
import Control.Monad.State
import qualified Text.ParserCombinators.Parsec as P
import qualified Data.Map as M

import Types


data MacroChar = MacroChar Char
               | DispatchMacroChar Char
    deriving (Show)

type ReadFn = P.Parser [LispVal]
data ReadMacro = ReadMacro { delimiter :: Char
                           , readFn    :: ReadFn
                           }


type DispatchTable = M.Map Char ReadMacro

type Reader = State DispatchTable LispVal

{-initTable :: DispatchTable
initTable = M.fromList [  ('(', )
                       ]-}

runReader :: Reader -> DispatchTable -> (LispVal, DispatchTable)
runReader = runState

addReadMacro :: DispatchTable -> Char -> ReadMacro -> Reader
addReadMacro dt c rm@(ReadMacro delimiter func) =
    case M.lookup c dt of
        Nothing -> do put (M.insert delimiter rm dt)
                      return $ Bool True
        Just _  -> return $ Bool False

getReadFn :: DispatchTable -> Char -> Maybe ReadFn
getReadFn t c = liftM readFn $ M.lookup c t

