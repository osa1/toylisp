module Reader where

import Control.Monad.ST
import Control.Monad.State
import qualified Data.Map as M

import Types


data ReadMacro = ReadMacro { openChar  :: Char
                           , closeChar :: Char
                           , readFunc  :: LispVal -> Reader
                           }

type DispatchTable = M.Map Char ReadMacro

type Reader = State DispatchTable LispVal

runReader :: Reader -> DispatchTable -> (LispVal, DispatchTable)
runReader = runState

