module Lisp where

import Types
import Parser

import Text.ParserCombinators.Parsec (parse)
import Debug.Trace

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

globalEnv :: Map.Map String Val
globalEnv = Map.fromList [ ("quote", VFunc $ SPure quote)
                         , ("car", VFunc $ Pure car)
                         , ("cdr", VFunc $ Pure cdr)
                         , ("cons", VFunc $ Pure cons)
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


-- still easier than state monad lol
evalList :: [Sexp] -> Env -> ([Val], Env)
evalList sexps env =
    iter sexps env []
    where iter sexps env vals =
            if (length sexps) == 0
               then (vals, env)
               else let (exp, newEnv) = eval (head sexps) env
                    in iter (tail sexps) newEnv (vals ++ [exp])


apply :: Val -> [Sexp] -> Env -> (Val, Env)
--apply a b c | trace ("apply " ++ show a ++ " exprs: " ++ show b) False = undefined
--apply (VFunc func) exprs env =
--    case func of
--        (Pure f) ->
--            let (vals, newEnv) = evalList exprs env
--                r = trace ("vals: " ++ show vals) $ f (toParams vals)
--            in (r, globalEnv)
--        otherwise -> trace "otherwise" (VSymbol "ok", globalEnv)
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
            let (vals, env1) = evalList exprs env
            in (f (toParams vals), env1)

        -- (Params Sexp -> Val)
        (SPure f) -> (f (toParams exprs), env)

toParams :: [a] -> Params a
toParams [a,b] = P2 a b
toParams [a]   = P1 a

repl :: IO ()
repl = do
    putStr "> "
    line <- getLine
    case parse sexp "" line of
        Right r -> let (val, env) = eval r globalEnv
                   in do putStrLn $ show val
                         repl
        Left r -> putStrLn "error"
    repl

main = repl

