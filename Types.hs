module Types where

import qualified Data.Map as Map

data Sexp = List [Sexp] | Atom String deriving (Show)

type Env = Map.Map String Func
data Func = SForm (Sexp -> Env -> (Val, Env)) | Func ([Val] -> Env -> (Val, Env))

data Val = VSymbol String | VList [Val] deriving (Show)