module Types
    ( LispVal(..)
    , unwordsList
    , nullEnv
    , Env
    , LispError(..)
    , ThrowsError
    , IOThrowsError
    , trapError
    , liftThrows
    , runIOThrows
    , extractValue
    , throwError
    ) where

import Control.Monad.Error
import Text.ParserCombinators.Parsec (ParseError(..))

import Data.IORef


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Character Char
             | Float Float
             | Bool Bool

instance Show LispVal where
    show (String contents) = "\"" ++ contents ++ "\""
    show (Atom name) = name
    show (Number contents) = show contents
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (List contents) = "(" ++ unwordsList contents ++ ")"
    show (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []


-- Error types

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where
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

type ThrowsError = Either LispError
type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val