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
    , ThrowsError
    , IOThrowsError
    , trapError
    , liftThrows
    , runIOThrows
    , extractValue
    , throwError
    ) where

import Control.Monad.Error
import Text.ParserCombinators.Parsec (ParseError)
import IO (Handle)

import Data.IORef


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Character Char
             | Float Float
             | Bool Bool

             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
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

data LispError = NumArgs Integer [LispVal]
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

type ThrowsError = Either LispError
type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

trapError :: IOThrowsError String -> IOThrowsError String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val