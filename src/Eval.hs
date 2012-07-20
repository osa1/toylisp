{-# LANGUAGE GADTs, NamedFieldPuns #-}
{-# OPTIONS_GHC -Wall -fno-warn-hi-shadowing -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}
module Eval where

import Control.Monad.Error (throwError)
import Data.Maybe (isNothing)
import Control.Monad.IO.Class (liftIO)

import Types
--import Prim
import Env

eval' :: Env -> AnyExpr -> Cont -> IOThrowsError TVal
eval' env (AnyExpr expr) = eval env expr

eval :: Env -> Expr a -> Cont -> IOThrowsError TVal
eval env (Symbol s) cont = getVar env s >>= applyCont cont env
eval env (Lambda params body) cont = makeNormalFunc env params body >>= applyCont cont env

-- Function application
eval env (Application (AnyExpr e) params) cont =
    eval env e (ApplyCont params [] cont)

eval env (Define (Symbol name) body) cont = eval' env body (DefineCont name cont)
eval env (Set (Symbol name) body) cont = eval' env body (SetCont name cont)

eval env (If pred thenE elseE) cont = eval' env pred (PredCont thenE elseE cont)

-- TODO: fexprs
eval env (Fexpr params body) cont = makeFexpr params body >>= applyCont cont env

eval env (Val v) cont = applyCont cont env v

eval env (List []) cont = applyCont cont env (TList [])
eval env (List (x:xs)) cont =
    eval' env x (SeqCont xs [] cont)

eval env (CallCC (Left (Symbol fun))) cont =
    getVar env fun >>= applyCont (ApplyCont [] [Continuation cont] cont) env
eval env (CallCC (Right lambda)) cont =
    eval env lambda (ApplyCont [] [Continuation cont] cont)

apply :: Env -> TVal -> [TVal] -> Cont -> IOThrowsError TVal
apply env (PrimFunc fun) args cont = fun args >>= applyCont cont env

apply _ (Func params varargs body closure) args cont =
    if length params /= length args && isNothing varargs
        then throwError $ NumArgs (length params) (length args)
        else liftIO (bindVars closure $ zip (map (\(Symbol s) -> s) params) args) >>=
            bindVarArgs varargs >>= evalBody
  where remainingArgs :: [TVal]
        remainingArgs = drop (length params) args

        bindVarArgs :: Maybe (Expr Symbol) -> Env -> IOThrowsError Env
        bindVarArgs args env = case args of
            Just (Symbol argName) -> liftIO $ bindVars env [(argName, TList remainingArgs)]
            Nothing -> return env

        evalBody :: Env -> IOThrowsError TVal
        evalBody env = applyCont (SeqLastCont body cont) env Nil

apply env (Continuation c) [param] _ = applyCont c env param
apply env (Continuation c) [] _ = applyCont c env Nil

apply env (PrimFexpr f) params cont = f (Env env) params cont
apply env (TFexpr params body) args cont =
    if length params /= length args then
        throwError $ NumArgs (length params) (length args)
    else
        liftIO (bindVars env $ zip (map (\(Symbol s) -> s) params) args) >>= evalBody
  where evalBody :: Env -> IOThrowsError TVal
        evalBody env = applyCont (SeqLastCont body cont) env Nil

apply _ wtf _ _ = throwError $ TypeMismatch FunctionType (typeOf wtf)


applyCont :: Cont -> Env -> TVal -> IOThrowsError TVal
applyCont EndCont _ val = return val
applyCont (PredCont _ elseE cont) env (Bool False) = eval' env elseE cont
applyCont (PredCont thenE _ cont) env _ = eval' env thenE cont

applyCont (ApplyCont args _ cont) env fexpr@TFexpr{} =
    apply env fexpr (map Syntax args) cont
applyCont (ApplyCont args _ cont) env fexpr@PrimFexpr{} =
    apply env fexpr (map Syntax args) cont

applyCont (ApplyCont (x:xs) args cont) env fun = do
    eval' env x (RemoveMeCont xs args fun cont)
applyCont (ApplyCont [] args cont) env fun = apply env fun args cont

applyCont (SeqCont (x:xs) vals cont) env val =
    eval' env x (SeqCont xs (vals ++ [val]) cont)
applyCont (SeqCont [] args cont) env val = applyCont cont env $ TList $ args ++ [val]

applyCont (RemoveMeCont xs args fun cont) env val =
    applyCont (ApplyCont xs (args ++ [val]) cont) env fun


applyCont (DefineCont name cont) env val = defineVar env name val >>= applyCont cont env
applyCont (SetCont name cont) env val = setVar env name val >>= applyCont cont env

applyCont (SeqLastCont (x:xs) cont) env _ =
    eval' env x (SeqLastCont xs cont)

applyCont (SeqLastCont [] cont) env v = applyCont cont env v

applyCont (EvalCont cont) env (Syntax s) = eval' env s cont
applyCont (EvalCont cont) env val = applyCont cont env val
