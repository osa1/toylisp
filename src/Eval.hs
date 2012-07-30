{-# LANGUAGE GADTs, NamedFieldPuns #-}
{-# OPTIONS_GHC -Wall -fno-warn-hi-shadowing -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}
module Eval where

import Control.Monad.Error (throwError)
import Data.Maybe (isNothing)
import Control.Monad.IO.Class (liftIO)

import Types
--import Prim
import Env

--evalForm :: TFexpr
--evalForm env [form] cont =
--    eval' env form cont
--evalForm _ args _ = throwError $ NumArgs 1 (length args)

eval' :: Env -> AnyExpr -> Cont -> IOThrowsError TVal
eval' env (AnyExpr expr) = eval env expr

eval :: Env -> Expr a -> Cont -> IOThrowsError TVal
eval env (Symbol s) cont = getVar env s >>= applyCont cont
eval env (Lambda params body) cont = makeNormalFunc env params body >>= applyCont cont

-- Function application
eval env (Application (AnyExpr e) params) cont =
    eval env e (ApplyCont params [] env cont)

eval env (Define (Symbol name) body) cont = eval' env body (DefineCont name env cont)
eval env (Set (Symbol name) body) cont = eval' env body (SetCont name env cont)

eval env (If pred thenE elseE) cont = eval' env pred (PredCont thenE elseE env cont)

-- TODO: fexprs
eval _ (Fexpr params body) cont = makeFexpr params body >>= applyCont cont

eval _ (Val v) cont = applyCont cont v

eval _ (List []) cont = applyCont cont (TList [])
eval env (List (x:xs)) cont =
    eval' env x (SeqCont xs [] env cont)

eval env (CallCC (Left (Symbol fun))) cont =
    getVar env fun >>= applyCont (ApplyCont [] [Continuation cont] env cont)
eval env (CallCC (Right lambda)) cont =
    eval env lambda (ApplyCont [] [Continuation cont] env cont)

apply :: TVal -> [TVal] -> Cont -> IOThrowsError TVal
apply (PrimFunc fun) args cont = fun args >>= applyCont cont

apply (Func params varargs body closure) args cont =
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
        evalBody env = applyCont (SeqLastCont body env cont) Nil

apply (Continuation c) [param] _ = applyCont c param
apply (Continuation c) [] _ = applyCont c Nil

apply PrimFexpr{} _ _ = throwError $ Default "fexpr application is not yet implemented."
apply TFexpr{} _ _ = throwError $ Default "fexpr application is not yet implemented."

apply wtf _ _ = throwError $ TypeMismatch FuncTy (typeOf wtf)

applyFexpr :: TVal -> [TVal] -> Env -> Cont -> IOThrowsError TVal
applyFexpr (TFexpr params body) args env cont =
    if length params /= length args then
        throwError $ NumArgs (length params) (length args)
    else
        liftIO (bindVars env $ zip (map (\(Symbol s) -> s) params) args) >>= evalBody
  where evalBody :: Env -> IOThrowsError TVal
        evalBody env = applyCont (SeqLastCont body env cont) Nil

applyFexpr notTFexpr _ _ _ = throwError $ TypeMismatch FexprTy (typeOf notTFexpr)


applyCont :: Cont -> TVal -> IOThrowsError TVal
applyCont EndCont val = return val
applyCont (PredCont _ elseE env cont) (Bool False) = eval' env elseE cont
applyCont (PredCont thenE _ env cont) _ = eval' env thenE cont

-- TODO: remove RemoveMe
--applyCont ApplyCont{} TFexpr{} = throwError $ Default "continuation application on Fexprs is not yet implemented"
applyCont (ApplyCont args _ env cont) fexpr@TFexpr{} =
    applyFexpr fexpr (map Syntax args) env cont
--applyCont ApplyCont{} fexpr@TFexpr{} = applyFexpr
applyCont (ApplyCont args _ env cont) (PrimFexpr fexpr) =
    fexpr (Env env) (map Syntax args) cont
--    liftIO $ putStrLn "fexpr application"
--    fexpr (Env env) (map Syntax args) cont
applyCont (ApplyCont (x:xs) args env cont) fun = do
    eval' env x (BindApplyCont xs args fun env cont)
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
