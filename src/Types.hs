{-# LANGUAGE NoMonomorphismRestriction, GADTs, NamedFieldPuns #-}
{-# OPTIONS_GHC -Wall -fno-warn-hi-shadowing -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}

module Types where

import Control.Monad.Error (Error(..), runErrorT, ErrorT(..))
import Text.ParserCombinators.Parsec (ParseError)
import Data.List (intercalate)

import Data.IORef (IORef, newIORef)
import qualified Data.Map as M

type Env = IORef [(String, IORef TVal)]

type GlobalEnv = IORef (M.Map String (IORef TVal))
type NamedEnv = (GlobalEnv, [[(String, TVal)]])

type TypedGlobalEnv = IORef (M.Map String TType)

--data TypedEnv a = (TypedGlobalEnv, [[(String, a)]])
type TypedEnv = (TypedGlobalEnv, [[(String, TType)]])

nullEnv :: IO Env
nullEnv = newIORef Nil >>= \ref -> newIORef [("nil", ref)]

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
    Lambda :: [(Expr Symbol, TType)] -> [AnyExpr] -> Expr Lambda
    Application :: AnyExpr -> [AnyExpr] -> Expr Application
    If :: AnyExpr -> AnyExpr -> AnyExpr -> Expr If
    Fexpr :: [(Expr Symbol, TType)] -> [AnyExpr] -> Expr Fexpr
    Val :: TVal -> Expr Val
    List :: [AnyExpr] -> Expr List

    -- Special forms
    CallCC :: Either (Expr Symbol) (Expr Lambda) -> Expr CallCC

    Define :: Expr Symbol -> AnyExpr -> Expr Define
    Set :: Expr Symbol -> AnyExpr -> Expr Set

type PrimFunc = [TVal] -> IOThrowsError TVal
type TFexpr = TVal -> [TVal] -> Cont -> IOThrowsError TVal
type PrimFexpr = TVal -> [TVal] -> Cont -> IOThrowsError TVal
type TMacro = Expr List -> Expr List

data AnyExpr where
    AnyExpr :: Expr a -> AnyExpr

instance Show AnyExpr where
    show (AnyExpr a) = show a

data TType = CharTy | StringTy | SymbolTy | IntTy | FloatTy | FuncTy [(String, TType)] TType
    | LstTy | BoolTy | ContTy | StxType | FexprTy | EnvTy | NilTy
  deriving (Show, Eq)

data TFunc = Func { params :: [(Expr Symbol, TType)]
                  , varargs :: Maybe (Expr Symbol)
                  , body :: [AnyExpr]
                  , closure :: Env
                  , ret :: TType
                  }
           | PrimFunc PrimFunc

data TVal = Char Char
          | String String
          | TSymbol String
          | Int Int
          | Float Float
          | TFunc TFunc
          -- | Func { params :: [Expr Symbol]
          --        , varargs :: Maybe (Expr Symbol)
          --        , body :: [AnyExpr]
          --        , closure :: Env
          --        }
          -- | PrimFunc PrimFunc
          -- | PrimFexpr PrimFexpr
          -- | TFexpr { fexprParams :: [Expr Symbol]
          --          , fexprBody :: [AnyExpr]
          --          }
          | TList [TVal]
          | Bool Bool
          | Continuation Cont
          | Syntax AnyExpr
          | Env Env
          | Nil
  --deriving (Eq) -- I need some custom equality rules

class Typed e where
    typeOf :: e -> TType

instance Typed TVal where
    typeOf Char{} = CharTy
    typeOf String{} = StringTy
    typeOf TSymbol{} = SymbolTy
    typeOf Int{} = IntTy
    typeOf Float{} = FloatTy
    typeOf TFunc{} = FuncTy [] NilTy
    --typeOf PrimFunc{} = FuncTy
    --typeOf PrimFexpr{} = FexprTy
    --typeOf TFexpr{} = FexprTy
    typeOf TList{} = LstTy
    typeOf Bool{} = BoolTy
    typeOf Continuation{} = ContTy
    typeOf Syntax{} = StxType
    typeOf Env{} = EnvTy
    typeOf Nil{} = NilTy


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


-- Function constructors
--makeFunc :: Maybe (Expr Symbol) -> Env -> [Expr Symbol] -> [AnyExpr] -> IOThrowsError TVal
--makeFunc varargs env params body = return $ TFunc (Func params varargs body env)
makeFunc = undefined

makeNormalFunc :: Env -> [Expr Symbol] -> [AnyExpr] -> IOThrowsError TVal
makeNormalFunc = makeFunc Nothing

makeVarargs :: Expr Symbol -> Env -> [Expr Symbol] -> [AnyExpr] -> IOThrowsError TVal
makeVarargs = makeFunc . Just

-- Fexpr constructor
--makeFexpr :: [Expr Symbol] -> [AnyExpr] -> IOThrowsError TVal
--makeFexpr params body = return $ TFexpr params body

-- Continuations

data Cont = EndCont
          | PredCont AnyExpr AnyExpr Env Cont
          -- | TestCont AnyExpr AnyExpr Env Cont
          | SetCont String Env Cont
          | DefineCont String Env Cont
          -- Function application
          | ApplyCont [AnyExpr] [TVal] Env Cont
          | SeqCont [AnyExpr] [TVal] Env Cont
          | BindApplyCont [AnyExpr] [TVal] TVal Env Cont
          | SeqLastCont [AnyExpr] Env Cont
          | EvalCont Env Cont


-- Show instances ------------------------------------------------------------------

instance Show (Expr a) where
    show (Symbol str) = str
    show (Lambda params body) = "(lambda (" ++ unwords (map show params) ++ ") " ++ unwords (map show body) ++ ")"
    --show (Application (Left fun) params) = "(" ++ show fun ++ " " ++ unwords (map show params) ++ ")"
    --show (Application (Right lambda) params) = "(" ++ show lambda ++ " " ++ unwords (map show params) ++ ")"
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
    show (TSymbol s) = s
    show (Int i) = show i
    show (Float f) = show f
    show TFunc{} = "<Function>"
    --show PrimFunc{} = "<Function>"
    --show PrimFexpr{} = "<Fexpr>"
    --show TFexpr{} = "<Fexpr>"
    show (TList l) = show l
    show (Bool b) = show b
    show Continuation{} = "<Continuation>"
    show Syntax{} = "<Syntax>"
    show Env{} = "<Env>"
    show Nil = "Nil"
