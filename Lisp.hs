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
    let f = eval (lst !! 0)
        args = drop 1 lst
    in apply f args env
eval (Atom atom) env =
    case Map.lookup atom env of
        Just v -> (v, env)
        Nothing -> (undefinedVal, env)


apply = undefined

