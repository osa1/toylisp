{-# OPTIONS_GHC -Wall
                -fno-warn-missing-signatures
                -fno-warn-hi-shadowing
                -fno-warn-unused-do-bind
                -fno-warn-name-shadowing #-}
{-# LANGUAGE GADTs #-}

module TC where

import Prelude
import Types hiding (typeOf)
import qualified Data.Map as M
import Data.IORef (readIORef, modifyIORef, newIORef, writeIORef)
import Control.Monad (liftM, liftM2)
import Data.Maybe (isNothing)
import Control.Monad.Error

import Parser
import Text.ParserCombinators.Parsec

type TypeError = String

type IOTypeError = ErrorT TypeError IO

newTypedEnv :: IO TypedEnv
newTypedEnv = do
    env <- newIORef $ M.fromList [("+", FuncTy [("i1", IntTy), ("i2", IntTy)] IntTy)]
    return (env, [])

lookup :: TypedEnv -> String -> IOTypeError TType
lookup (globalenv, scope) s = do
    globenv <- liftIO $ readIORef globalenv
    case M.lookup s globenv of
        Just t -> return t
        Nothing -> searchScope scope s
  where searchScope :: [[(String, TType)]] -> String -> IOTypeError TType
        searchScope [] _ = throwError $ "unbound var " ++ s
        searchScope (x:xs) name = case Prelude.lookup name x of
                                      Nothing -> searchScope xs name
                                      Just t -> return t

addLocalBindings :: TypedEnv -> [(String, TType)] -> TypedEnv
addLocalBindings (global,scope) bindings = (global, bindings:scope)

addGlobalBinding :: TypedEnv -> (String, TType) -> IO ()
addGlobalBinding (global,_) (name,ty) = do
    env <- readIORef global
    let env' = M.insert name ty env
    writeIORef global env'

typeOf :: TypedEnv -> AnyExpr -> IOTypeError TType
typeOf env (AnyExpr (Symbol s)) = TC.lookup env s
typeOf env (AnyExpr (Lambda params body)) = do
    let env' = addLocalBindings env (map (\((Symbol s), ty) -> (s, ty)) params)
    liftM last $ checkSeq env' body
typeOf env (AnyExpr (Application x xs)) = do
    ft <- typeOf env x
    case ft of
        (FuncTy params ret) -> do
            ptys <- checkSeq env xs
            if (map snd params) == ptys then return ret else throwError "type error on parameters"
        _ -> throwError "Application to a non-function data"
typeOf env (AnyExpr (If guard thenE elseE)) = do
    guardty <- typeOf env guard
    if guardty /= BoolTy then
        throwError "Guard is not boolean type"
    else do
        thenty <- typeOf env thenE
        elsety <- typeOf env elseE
        if thenty == elsety then return thenty else throwError "then and else expression are not same type"
typeOf _ (AnyExpr (Val tval)) = return $ case tval of
    Char{} -> CharTy
    String{} -> StringTy
    TSymbol{} -> SymbolTy
    Int{} -> IntTy
    Float{} -> FloatTy
    TFunc{} -> FuncTy [] NilTy
    TList{} -> LstTy
    Bool{} -> BoolTy
    Continuation{} -> ContTy
    Syntax{} -> StxType
    Env{} -> EnvTy
    Nil{} -> NilTy
typeOf _ expr = throwError $ "not yet implemented: " ++ show expr

checkSeq :: TypedEnv -> [AnyExpr] -> IOTypeError [TType]
checkSeq env exprs = sequence $ map (typeOf env) exprs

-- tests -----------------------------------------------

runTC :: IOTypeError TType -> IO (Either TypeError TType)
runTC action = do
    r <- runErrorT action
    return r

main :: IO ()
main = do
    let expr = parse parseAnyExpr "tc test" "(lambda (x : int) (+ y 1))"
    env <- newTypedEnv
    addGlobalBinding env ("y", BoolTy)
    case expr of
        Left err -> putStrLn (show err)
        Right e -> do
          r <- runTC $ typeOf env e
          putStrLn $ show r
          return ()








