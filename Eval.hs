{-# LANGUAGE GADTs, NamedFieldPuns #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns
                -fwarn-unused-binds
                -fwarn-unused-matches #-}
module Eval where

import Control.Monad.Error (throwError)
import Data.Maybe (isNothing)
import Control.Monad.IO.Class (liftIO)

import Types
import Prim
import Env

--evalForm :: TFexpr
--evalForm env [Syntax exprs] = eval' env exprs EndCont
--evalForm _ [notSyntax] = throwError $ TypeMismatch SyntaxType (typeOf notSyntax)
--evalForm _ params = throwError $ NumArgs 1 (length params)

eval' :: Env -> AnyExpr -> Cont -> IOThrowsError TVal
eval' env (AnyExpr expr) = eval env expr

eval :: Env -> Expr a -> Cont -> IOThrowsError TVal
eval env (Symbol s) cont = getVar env s >>= applyCont cont
eval env (Lambda params body) cont = makeNormalFunc env params body >>= applyCont cont

-- Function application
eval env (Application (Left (Symbol fun)) params) cont =
    getVar env fun >>= applyCont (ApplyCont params [] env cont)
eval env (Application (Right lambda) params) cont =
    eval env lambda (ApplyCont params [] env cont)

eval env (Define (Symbol name) body) cont = eval' env body (DefineCont name env cont)
eval env (Set (Symbol name) body) cont = eval' env body (SetCont name env cont)

eval env (If pred thenE elseE) cont = eval' env pred (PredCont thenE elseE env cont)

-- TODO: fexprs
eval _ (Fexpr _ _) _ = do
    liftIO $ putStrLn "Fexprs are not yet implemented."
    undefined

eval _ (Val v) cont = applyCont cont v
eval _ (List _) _ = do
    liftIO $ putStrLn "Lists are not yet implemented."
    undefined -- TODO: why did I add this?

eval env (EvalExp form) cont = eval' env form cont

eval env (CallCC (Left (Symbol fun))) cont =
    getVar env fun >>= applyCont (ApplyCont [] [Continuation cont] env cont)
eval env (CallCC (Right lambda)) cont =
    eval env lambda (ApplyCont [] [Continuation cont] env cont)

apply :: TVal -> [TVal] -> Cont -> IOThrowsError TVal
apply (SimpleFunc fun) args cont = fun args >>= applyCont cont

apply (Func params varargs body closure) args cont =
    if num params /= num args && isNothing varargs
        then throwError $ NumArgs (length params) (length args)
        else liftIO (bindVars closure $ zip (map (\(Symbol s) -> s) params) args) >>=
            bindVarArgs varargs >>= evalBody
  where remainingArgs :: [TVal]
        remainingArgs = drop (length params) args

        num :: [a] -> Integer
        num = toInteger . length

        bindVarArgs :: Maybe (Expr Symbol) -> Env -> IOThrowsError Env
        bindVarArgs args env = case args of
            Just (Symbol argName) -> liftIO $ bindVars env [(argName, TList remainingArgs)]
            Nothing -> return env

        evalBody :: Env -> IOThrowsError TVal
        evalBody env = applyCont (SeqLastCont body env cont) Nil
        -- FIXME: maybe I should completely remove last parameter of applyCont

apply (Continuation c) [param] _ = applyCont c param
apply (Continuation c) [] _ = applyCont c Nil

apply wtf _ _ = throwError $ TypeMismatch FunctionType (typeOf wtf)


applyCont :: Cont -> TVal -> IOThrowsError TVal
applyCont EndCont val = return val
applyCont (PredCont _ elseE env cont) (Bool False) = eval' env elseE cont
applyCont (PredCont thenE _ env cont) _ = eval' env thenE cont

applyCont (ApplyCont (x:xs) args env cont) fun = do
    r <- eval' env x EndCont
    applyCont (ApplyCont xs (args++[r]) env cont) fun
-- TODO: be sure this works as expected
applyCont (ApplyCont [] args _ cont) fun = apply fun args cont

applyCont (DefineCont name env cont) val = defineVar env name val >>= applyCont cont
applyCont (SetCont name env cont) val = setVar env name val >>= applyCont cont

-- FIXME: there are some errors in SeqLastCont
-- Try this:
     --(define cont #f)
     --(define test () ((lambda (i) (call/cc (lambda (k) (set! cont k))) (set! i (+ i 1)) i) 0))
     --(cont)
applyCont (SeqLastCont (x:xs) env cont) _ =
    eval' env x (SeqLastCont xs env cont)

applyCont (SeqLastCont [] _ cont) v = applyCont cont v
