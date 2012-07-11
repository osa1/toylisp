{-# LANGUAGE NoMonomorphismRestriction, GADTs, NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-hi-shadowing #-}

--module Types
--    ( LispVal(..)
--    , unwordsList
--    , nullEnv
--    , Env
--    , makeFunc
--    , makeNormalFunc
--    , makeVarargs
--    , LispError(..)
--    , IOThrowsError
--    , runIOThrows
--    , throwError
--    , Cont(..)
--    ) where

module Types where

import Control.Monad.Error
import Text.ParserCombinators.Parsec (ParseError)
import System.IO (Handle)
import Data.List (intercalate)

import Data.IORef

type Env = IORef [(String, IORef Val)]

nullEnv :: IO Env
nullEnv = newIORef []

data Symbol
data Lambda
data Application
data If
data Fexpr
data Val
data EvalExp
data CallCC

data Expr a where
    Symbol :: String -> Expr Symbol
    Lambda :: [Expr Symbol] -> [AnyExpr] -> Expr Lambda
    Application :: Either (Expr Symbol) (Expr Lambda) -> [AnyExpr] -> Expr Application
    If :: AnyExpr -> AnyExpr -> AnyExpr -> Expr If
    Fexpr :: [Expr Symbol] -> [AnyExpr] -> Expr Fexpr
    Val :: TVal -> Expr Val

    -- Special forms
    EvalExp :: AnyExpr -> Expr EvalExp
    CallCC :: Either (Expr Symbol) (Expr Lambda) -> [AnyExpr] -> Expr CallCC

instance Show (Expr a) where
    show (Symbol str) = str
    show (Lambda params body) = "(lambda (" ++ intercalate " " (map show params) ++ ") " ++ intercalate " " (map show body) ++ ")"
    show (Application (Left fun) params) = "(" ++ show fun ++ " " ++ intercalate " " (map show params) ++ ")"
    show (Application (Right lambda) params) = "(" ++ show lambda ++ " " ++ intercalate " " (map show params) ++ ")"
    show (If ifE thenE elseE) = "(" ++ intercalate "," [show ifE, show thenE, show elseE] ++ ")"
    show (Fexpr params body) = "(fexpr (" ++ intercalate " " (map show params) ++ ") " ++ intercalate " " (map show body) ++ ")"
    show (Val tval) = show tval

    show (EvalExp expr) = "(eval " ++ show expr ++ ")"
    show (CallCC (Left fun) exprs) = show "(call/cc " ++ show fun ++ intercalate " " (map show exprs) ++ ")"
    show (CallCC (Right lambda) exprs) = show "(call/cc " ++ show lambda ++ intercalate " " (map show exprs) ++ ")"


data Func = Func { params :: [Expr Symbol]
                 , varargs :: Maybe (Expr Symbol)
                 , body :: [AnyExpr]
                 , closure :: Env
                 }

instance Show Func where
    show Func{params} = "<Function " ++ show params ++ " >"

data AnyExpr where
    AnyExpr :: Expr a -> AnyExpr

instance Show AnyExpr where
    show (AnyExpr a) = show a

data TVal = Char Char
          | String String
          | Int Int
          | Float Float
          | Function Func
          | List [AnyExpr]
          | Bool Bool
          | Nil
    deriving Show


-- Continuations

--data Cont = EndCont
--          | PredCont LispVal LispVal Env Cont
--          | TestCont LispVal LispVal Env Cont
--          | SetCont String Env Cont
--          | DefineCont String Env Cont
--          | SeqCont [LispVal] [LispVal] Env Cont
--          | SeqLastCont [LispVal] Env Cont
--          | ArgsCont [LispVal] [LispVal] Env Cont



--data LispVal = Atom String
--             | List [LispVal]
--             | DottedList [LispVal] LispVal
--             | Number Integer
--             | String String
--             | Character Char
--             | Float Float
--             | Bool Bool

--             | PrimitiveFunc ([LispVal] -> IOThrowsError LispVal)
--             | Func { params :: [String]
--                    , vararg :: Maybe String
--                    , body :: [LispVal]
--                    , closure :: Env
--                    }

--             | IOFunc ([LispVal] -> IOThrowsError LispVal)
--             | Port Handle

--             | Continuation Cont

--instance Show LispVal where
--    show (String contents) = "\"" ++ contents ++ "\""
--    show (Character char) = "'" ++ [char] ++ "'"
--    show (Atom name) = name
--    show (Number contents) = show contents
--    show (Float float) = show float
--    show (Bool True) = "#t"
--    show (Bool False) = "#f"
--    show (List contents) = "(" ++ unwordsList contents ++ ")"
--    show (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"
--    show (PrimitiveFunc _) = "<primitive>"
--    show (Func args varargs _ _) =
--        "(lambda (" ++ unwords (map show args) ++
--            (case varargs of
--                Nothing -> ") ...)"
--                Just arg -> " . " ++ arg ++ ") ...)")
--    show (Port _) = "<IO port>"
--    show (IOFunc _) = "<IO primitive>"
--    show (Continuation _) = "<Continuation>"

--makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
--makeFunc varargs env params body = return $ Func (map show params) varargs body env

--makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
--makeNormalFunc = makeFunc Nothing

--makeVarargs :: (Show a) => a -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
--makeVarargs = makeFunc . Just . show

--unwordsList :: [LispVal] -> String
--unwordsList = unwords . map show


---- Error types

--data LispError = NumArgs Int [LispVal]
--               | TypeMismatch String LispVal
--               | Parser ParseError
--               | BadSpecialForm String LispVal
--               | NotFunction String String
--               | UnboundVar String String
--               | Default String

--instance Show LispError where
--    show (Default message) = message
--    show (UnboundVar message varname) = message ++ ": " ++ varname
--    show (BadSpecialForm message form) = message ++ ": " ++ show form
--    show (NotFunction message func) = message ++ ": " ++ show func
--    show (NumArgs expected found) = "Expected " ++ show expected ++
--                                    " args: found values " ++ unwordsList found
--    show (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++
--                                         ", found " ++ show found
--    show (Parser parseErr) = "Parse error at " ++ show parseErr

--instance Error LispError where
--    noMsg = Default "An error has occured"
--    strMsg = Default

--type IOThrowsError = ErrorT LispError IO

---- used in REPL
--runIOThrows :: IOThrowsError String -> IO String
--runIOThrows action = do
--    r <- runErrorT action
--    return $ extractValue r

--extractValue :: Either LispError String -> String
--extractValue (Right val) = val
--extractValue (Left err) = show err


---- Continuations

--data Cont = EndCont
--          | PredCont LispVal LispVal Env Cont
--          | TestCont LispVal LispVal Env Cont
--          | SetCont String Env Cont
--          | DefineCont String Env Cont
--          | SeqCont [LispVal] [LispVal] Env Cont
--          | SeqLastCont [LispVal] Env Cont
--          | ArgsCont [LispVal] [LispVal] Env Cont


