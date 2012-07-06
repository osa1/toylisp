{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-hi-shadowing #-}

module Types
    ( LispVal(..)
    , unwordsList
    , nullEnv
    , Env
    , makeFunc
    , makeNormalFunc
    , makeVarargs
    , LispError(..)
    , IOThrowsError
    , runIOThrows
    , throwError
    , Cont(..)
    ) where

import Control.Monad.Error
import Text.ParserCombinators.Parsec (ParseError)
import System.IO (Handle)

import Data.IORef
import qualified Data.Set as S


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Character Char
             | Float Float
             | Bool Bool

             | PrimitiveFunc ([LispVal] -> IOThrowsError LispVal)
             | Func { params :: [String]
                    , vararg :: (Maybe String)
                    , body :: [LispVal]
                    , closure :: Env
                    }

             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

instance Show LispVal where
    show (String contents) = "\"" ++ contents ++ "\""
    show (Character char) = "'" ++ [char] ++ "'"
    show (Atom name) = name
    show (Number contents) = show contents
    show (Float float) = show float
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (List contents) = "(" ++ unwordsList contents ++ ")"
    show (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"
    show (PrimitiveFunc _) = "<primitive>"
    show (Func args varargs _ _) =
        "(lambda (" ++ unwords (map show args) ++
            (case varargs of
                Nothing -> ") ...)"
                Just arg -> " . " ++ arg ++ ") ...)")
    show (Port _) = "<IO port>"
    show (IOFunc _) = "<IO primitive>"

makeFunc :: (Maybe String) -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs env params body = return $ Func (map show params) varargs body env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVarargs :: (Show a) => a -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarargs = makeFunc . Just . show

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []


-- Error types

data LispError = NumArgs Int [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where
    show (Default message) = message
    show (UnboundVar message varname) = message ++ ": " ++ varname
    show (BadSpecialForm message form) = message ++ ": " ++ show form
    show (NotFunction message func) = message ++ ": " ++ show func
    show (NumArgs expected found) = "Expected " ++ show expected ++
                                    " args: found values " ++ unwordsList found
    show (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++
                                         ", found " ++ show found
    show (Parser parseErr) = "Parse error at " ++ show parseErr

instance Error LispError where
    noMsg = Default "An error has occured"
    strMsg = Default

type IOThrowsError = ErrorT LispError IO

-- used in REPL
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = do
    r <- runErrorT action
    -- r :: Either LispError String
    return $ extractValue r

extractValue :: Either LispError String -> String
extractValue (Right val) = val
extractValue (Left err) = show err


-- Continuations

data Cont = EndCont
          | PredCont LispVal LispVal Env Cont
          | TestCont LispVal LispVal Env Cont
          | SetCont String Env Cont
          | ApplyCont LispVal [LispVal] Env Cont -- Func, Params, env and last cont

