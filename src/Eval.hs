{-# LANGUAGE GADTs, NamedFieldPuns #-}
{-# OPTIONS_GHC -Wall -fno-warn-hi-shadowing -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}
module Eval where

import Control.Monad.Error (throwError)
import Data.Maybe (isNothing)
import Control.Monad.IO.Class (liftIO)

import Types
--import Prim
import Env

eval' :: TEnv -> AnyExpr -> Cont -> IOThrowsError TVal
eval' env (AnyExpr expr) = eval env expr

eval :: TEnv -> Expr a -> Cont -> IOThrowsError TVal
eval env (Symbol s) cont = getRef env s >>= applyCont cont
eval env (Lambda params _ body) cont = applyCont cont (makeNormalFunc env params body)

-- Function application
eval env (Application (AnyExpr e) params) cont =
    eval env e (ApplyCont params [] env cont)

eval env (Define (Symbol name) body) cont = eval' env body (DefineCont name env cont)
eval env (Set (Symbol name) body) cont = eval' env body (SetCont name env cont)

eval env (If pred thenE elseE) cont = eval' env pred (PredCont thenE elseE env cont)

eval _ (Val v) cont = applyCont cont v

eval _ (List []) cont = applyCont cont (TList [])
eval env (List (x:xs)) cont =
    eval' env x (SeqCont xs [] env cont)

eval env (CallCC (Left (Symbol fun))) cont =
    getRef env fun >>= applyCont (ApplyCont [] [Continuation cont] env cont)
eval env (CallCC (Right lambda)) cont =
    eval env lambda (ApplyCont [] [Continuation cont] env cont)

apply :: TVal -> [TVal] -> Cont -> IOThrowsError TVal
apply (TFunc (PrimFunc fun _)) args cont = fun args >>= applyCont cont

apply (TFunc (Func params varargs body closure _)) args cont =
    if length params /= length args && isNothing varargs
        then throwError $ NumArgs (length params) (length args)
        else do env <- liftIO $ addLocalRefs closure $ zip (map (\(Symbol s,_) -> s) params) args
                bindVarArgs varargs env >>= evalBody
  where remainingArgs :: [TVal]
        remainingArgs = drop (length params) args

        bindVarArgs :: Maybe (Expr Symbol) -> TEnv -> IOThrowsError TEnv
        bindVarArgs args env = case args of
            Just (Symbol argName) -> liftIO $ addLocalRefs env [(argName, TList remainingArgs)]
            Nothing -> return env

        evalBody :: TEnv -> IOThrowsError TVal
        evalBody env = applyCont (SeqLastCont body env cont) Unit

apply (Continuation c) [param] _ = applyCont c param
apply (Continuation c) [] _ = applyCont c Unit


applyCont :: Cont -> TVal -> IOThrowsError TVal
applyCont EndCont val = return val
applyCont (PredCont _ elseE env cont) (Bool False) = eval' env elseE cont
applyCont (PredCont thenE _ env cont) _ = eval' env thenE cont

-- TODO: remove RemoveMe
applyCont (ApplyCont (x:xs) args env cont) fun = eval' env x (BindApplyCont xs args fun env cont)
applyCont (ApplyCont [] args _ cont) fun = apply fun args cont

applyCont (SeqCont (x:xs) vals env cont) val =
    eval' env x (SeqCont xs (vals ++ [val]) env cont)
applyCont (SeqCont [] args _ cont) val = applyCont cont $ TList $ args ++ [val]

applyCont (BindApplyCont xs args fun env cont) val =
    applyCont (ApplyCont xs (args ++ [val]) env cont) fun


applyCont (DefineCont name env cont) val = defineVar env name val >>= applyCont cont
applyCont (SetCont name env cont) val = setVar env name val >>= applyCont cont

applyCont (SeqLastCont (x:xs) env cont) _ =
    eval' env x (SeqLastCont xs env cont)

applyCont (SeqLastCont [] _ cont) v = applyCont cont v

applyCont (EvalCont env cont) (Syntax s) = eval' env s cont
applyCont (EvalCont _ cont) val = applyCont cont val
