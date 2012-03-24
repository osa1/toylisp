module Lisp where

import Types
import Parser

import Text.ParserCombinators.Parsec (parse)
import Debug.Trace

import qualified Data.Map as Map

undefinedVal = VSymbol "undefined"
t = VSymbol "T"
nil = VList []

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

equal :: Params Val -> Val
equal (P2 v1 v2) = case v1 == v2 of
                     True  -> t
                     False -> nil

atom :: Params Val -> Val
atom (P1 (VSymbol v)) = t
atom _                = nil

cond :: Params Sexp -> Env -> (Val, Env)
cond = undefined

lambda :: Params Sexp -> Val
lambda (P2 (List params) expr) =
    VFunc (Func f)
    where
        f :: (Params Val -> Env -> (Val, Env))
        --f = undefined
        f = (\args env ->
                let argList = toList args
                    --(argVals, newEnv) = evalList argList env
                    funcEnv = Env { env = Map.fromList (zip (map atomStr params) argList)
                                  , parentEnv = Just env }
                in eval expr funcEnv)
lambda _ = undefined

atomStr :: Sexp -> String
atomStr (Atom s) = s

globalEnv :: Env
globalEnv = Env (Map.fromList [ ("quote", VFunc $ SPure quote)
                              , ("car", VFunc $ Pure car)
                              , ("cdr", VFunc $ Pure cdr)
                              , ("cons", VFunc $ Pure cons)
                              , ("equal", VFunc $ Pure equal)
                              , ("atom", VFunc $ Pure atom)
                              , ("cond", VFunc $ SForm cond)
                              , ("lambda", VFunc $ SPure lambda)
                              ])
                 Nothing

envLookup :: String -> Env -> Maybe Val
envLookup key (Env envTable parentEnv) =
    case Map.lookup key envTable of
      Just v  -> Just v
      Nothing ->
        case parentEnv of
          Just e  -> envLookup key e
          Nothing -> Nothing

eval :: Sexp -> Env -> (Val, Env)
eval (List lst) env =
    let (f, newEnv) = eval (lst !! 0) env
        args = drop 1 lst
    in apply f args newEnv
eval (Atom atom) env =
    case envLookup atom env of
        Just v  -> (v, env)
        Nothing -> (undefinedVal, env)


-- still easier than working with state monad lol
evalList :: [Sexp] -> Env -> ([Val], Env)
evalList sexps env =
    iter sexps env []
    where iter sexps env vals =
            if (length sexps) == 0
               then (vals, env)
               else let (exp, newEnv) = eval (head sexps) env
                    in iter (tail sexps) newEnv (vals ++ [exp])


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
            let (vals, env1) = evalList exprs env
            in (f (toParams vals), env1)

        -- (Params Sexp -> Val)
        (SPure f) -> (f (toParams exprs), env)

toParams :: [a] -> Params a
toParams [a,b] = P2 a b
toParams [a]   = P1 a

toList :: Params a -> [a]
toList (P1 a) = [a]
toList (P2 a b) = [a, b]

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

