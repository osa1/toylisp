module Lisp where

import Types
import Parser

import qualified Data.Map as Map

undefinedVal = VSymbol "undefined"

quote :: Params Sexp -> Val
quote (P1 (List sexps))  = VList $ map (quote . P1) sexps
quote (P1 (Atom symbol)) = VSymbol symbol

car :: Params Val -> Val
car (P1 (VList (x:rest))) = x

cdr :: Params Val -> Val
cdr (P1 (VList (x:rest))) = VList rest

cons :: Params Val -> Val
cons (P2 v1 v2) = case v2 of
                    (VList lst) -> VList (v1:lst)
                    otherwise   -> VList [v1, v2]

globalEnv = Map.fromList [ ("quote", SPure quote)
                         , ("car", Pure car)
                         , ("cdr", Pure cdr)
                         , ("cons", Pure cons)
                         ]

eval :: Sexp -> Env -> (Val, Env)
eval (List lst) env =
    let (f, newEnv) = eval (lst !! 0) env
        args = drop 1 lst
    in apply f args newEnv
eval (Atom atom) env =
    case Map.lookup atom env of
        Just v  -> (v, env)
        Nothing -> (undefinedVal, env)

evalList :: [Sexp] -> Env -> ([Val], Env)
evalList = undefined

apply :: Val -> [Sexp] -> Env -> (Val, Env)
apply (VFunc func) exprs env =
    case func of

        -- (Params Sexp -> Env -> (Val, Env))
        (SForm f) -> f (toParams exprs) env

        -- (Params Val -> Env -> (Val, Env))
        (Func f)  ->
            let (vals, lastEnv) = evalList exprs env
            in f (toParams vals) lastEnv

        -- (Params Val -> Val)
        (Pure f)  ->
            let (vals, env) = evalList exprs env
            in (f (toParams vals), env)

        -- (Params Sexp -> Val)
        (SPure f) -> (f (toParams exprs), env)

toParams :: [a] -> Params a
toParams [a,b] = P2 a b
toParams [a]   = P1 a




