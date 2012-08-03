{-# OPTIONS_GHC -Wall
                -fno-warn-hi-shadowing
                -fno-warn-name-shadowing
                -fno-warn-unused-do-bind #-}
{-# LANGUAGE NoMonomorphismRestriction, GADTs, NamedFieldPuns #-}

module Types where

import Control.Monad.Error (Error(..), runErrorT, ErrorT(..))
import Text.ParserCombinators.Parsec (ParseError)
import Data.List (intercalate)
import qualified Data.Map as M
import Data.IORef (IORef)

type Env a = (M.Map String a, [[(String, a)]])
type Binding = (TVal, TType)
type TEnv = Env (IORef TVal)

data Symbol
data Lambda
data Application
data If
data Fexpr
data Val
data List
data CallCC
data Define
data Set

data Expr a where
    Symbol :: String -> Expr Symbol
    Lambda :: [(Expr Symbol, TType)] -> TType -> [AnyExpr] -> Expr Lambda
    Application :: AnyExpr -> [AnyExpr] -> Expr Application
    If :: AnyExpr -> AnyExpr -> AnyExpr -> Expr If
    Fexpr :: [(Expr Symbol, TType)] -> [AnyExpr] -> Expr Fexpr
    Val :: TVal -> Expr Val
    List :: [AnyExpr] -> Expr List

    -- Special forms
    CallCC :: Either (Expr Symbol) (Expr Lambda) -> Expr CallCC

    Define :: Expr Symbol -> AnyExpr -> Expr Define
    Set :: Expr Symbol -> AnyExpr -> Expr Set

type TFexpr = TVal -> [TVal] -> Cont -> IOThrowsError TVal
type PrimFexpr = TVal -> [TVal] -> Cont -> IOThrowsError TVal
type TMacro = Expr List -> Expr List

data AnyExpr where
    AnyExpr :: Expr a -> AnyExpr

instance Show AnyExpr where
    show (AnyExpr a) = show a

data TType = CharTy | StringTy | IntTy | FloatTy | FuncTy [(String, TType)] TType
    | LstTy TType | BoolTy | ContTy | StxType | UnitTy
  deriving (Show, Eq)

data TFunc = Func { params :: [(Expr Symbol, TType)]
                  , varargs :: Maybe (Expr Symbol)
                  , body :: [AnyExpr]
                  , closure :: TEnv
                  , ret :: TType
                  }
           | PrimFunc { primF :: ([TVal] -> IOThrowsError TVal)
                      , ty :: TType
                      }

makeNormalFunc :: TEnv -> [(Expr Symbol, TType)] -> [AnyExpr] -> TVal
makeNormalFunc = undefined

data TVal = Char Char
          | String String
          | Int Int
          | Float Float
          | TFunc TFunc
          | TList [TVal]
          | Bool Bool
          | Continuation Cont
          | Syntax AnyExpr
          | Unit
  --deriving (Eq) -- I need some custom equality rules

-- Errors

data TError = NumArgs Int Int
            | TypeMismatch TType TType
            | Parser ParseError
            | BadSpecialForm String AnyExpr
            | NotFunc String String
            | UnboundVar String String
            | SyntaxError
            | Default String

instance Error TError where
    noMsg = Default "An error has occured"
    strMsg = Default

type IOThrowsError = ErrorT TError IO

-- used in REPL
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = do
    r <- runErrorT action
    return $ extractValue r

extractValue :: Either TError String -> String
extractValue (Right val) = val
extractValue (Left err) = show err

unwordsList :: [AnyExpr] -> String
unwordsList = unwords . map (\(AnyExpr e) -> show e)

-- Continuations

data Cont = EndCont
          | PredCont AnyExpr AnyExpr TEnv Cont
          | SetCont String TEnv Cont
          | DefineCont String TEnv Cont
          -- Function application
          | ApplyCont [AnyExpr] [TVal] TEnv Cont
          | SeqCont [AnyExpr] [TVal] TEnv Cont
          | BindApplyCont [AnyExpr] [TVal] TVal TEnv Cont
          | SeqLastCont [AnyExpr] TEnv Cont
          | EvalCont TEnv Cont


-- Show instances ------------------------------------------------------------------

instance Show (Expr a) where
    show (Symbol str) = str
    show (Lambda params ret body) = "(lambda (" ++ unwords (map show params) ++ ") : " ++ show ret ++ " " ++ unwords (map show body) ++ ")"
    show (Application f params) = "(" ++ show f ++ " " ++ unwords (map show params) ++ ")"
    show (If ifE thenE elseE) = "(" ++ intercalate "," [show ifE, show thenE, show elseE] ++ ")"
    show (Fexpr params body) = "(fexpr (" ++ unwords (map show params) ++ ") " ++ unwords (map show body) ++ ")"
    show (Val tval) = show tval
    show (List exprs) = "(" ++ unwords (map show exprs) ++ ")"

    show (CallCC (Left fun)) = show "(call/cc " ++ show fun ++ ")"
    show (CallCC (Right lambda)) = show "(call/cc " ++ show lambda ++ ")"

    show (Define (Symbol name) body) = show "(define " ++ name ++ " " ++ show body ++ ")"
    show (Set (Symbol name) body) = show "(set! " ++ name ++ " " ++ show body ++ ")"


instance Show TError where
    show (Default message) = message
    show (UnboundVar message varname) = message ++ ": " ++ varname
    show (BadSpecialForm message form) = message ++ ": " ++ show form
    show (NotFunc message func) = message ++ ": " ++ show func
    show (NumArgs expected found) = "Expected " ++ show expected ++
                                    " args: found " ++ show found
    show (TypeMismatch expected found) = "Invalid type: expected " ++ show expected ++
                                         ", found " ++ show found
    show (Parser parseErr) = "Parse error at " ++ show parseErr
    show SyntaxError = "Syntax error"


instance Show TVal where
    show (Char c) = "#\\" ++ [c]
    show (String s) = s
    show (Int i) = show i
    show (Float f) = show f
    show TFunc{} = "<Function>"
    show (TList l) = show l
    show (Bool b) = show b
    show Continuation{} = "<Continuation>"
    show Syntax{} = "<Syntax>"
    show Unit = "Unit"
