module Types where

import qualified Data.Map as Map

-- syntax
data Sexp = List [Sexp] | Atom String deriving (Show)

type Env = Map.Map String Val

data Func = SForm (Params Sexp -> Env -> (Val, Env)) -- cond like forms
          | Func (Params Val -> Env -> (Val, Env))   -- ordinary functions, with optional side-effects(defun)
          | SPure (Params Sexp -> Val)               -- macros?? quote-like forms
          | Pure (Params Val -> Val)                 -- pure functions, car, cdr, etc

data Params a = P1 a | P2 a a -- | P3 a a a

data Val = VSymbol String | VList [Val] | VFunc Func deriving (Show)

instance Show Func where
    show (SForm _) = "SForm"
    show (Func _)  = "Func"
    show (SPure _) = "SPure"
    show (Pure _)  = "Pure"
