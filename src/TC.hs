{-# OPTIONS_GHC -Wall
                -fno-warn-missing-signatures
                -fno-warn-hi-shadowing
                -fno-warn-unused-do-bind
                -fno-warn-name-shadowing #-}
{-# LANGUAGE GADTs, NamedFieldPuns #-}

module TC where

import Prelude
import Types
import qualified Data.Map as M
import Data.IORef (readIORef, modifyIORef, newIORef)
--import Control.Monad (liftM, liftM2)
import Control.Monad.Error

import Parser
import Text.ParserCombinators.Parsec

type TypedEnv = Env TType

class Typed e where
    typeOf :: TypedEnv -> e -> IOTypeError TType

instance Typed TVal where
    typeOf _ Char{} = return CharTy
    typeOf _ String{} = return StringTy
    typeOf _ Int{} = return IntTy
    typeOf _ Float{} = return FloatTy
    typeOf _ (TFunc Func{params,ret}) =
        return $ FuncTy (map (\((Symbol s),ty) -> (s, ty)) params) ret
    typeOf _ (TFunc PrimFunc{primFParams=params,primFRet=ret}) =
        return $ FuncTy (map (\((Symbol s),ty) -> (s, ty)) params) ret
    typeOf _ TList{} = return LstTy
    typeOf _ Bool{} = return BoolTy
    typeOf _ Continuation{} = return ContTy
    typeOf _ Syntax{} = return StxType
    typeOf _ Unit{} = return UnitTy

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
addGlobalBinding (global,_) (name,ty) =
    modifyIORef global $ \env -> M.insert name ty env

checkSeq :: Typed a => TypedEnv -> [a] -> ErrorT TypeError IO [TType]
checkSeq env exprs = sequence $ map (typeOf env) exprs

instance Typed AnyExpr where
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
  typeOf env (AnyExpr (Val tval)) = typeOf env tval
  typeOf _ expr = throwError $ "not yet implemented: " ++ show expr

instance Typed (Expr a) where
    typeOf env expr = typeOf env (AnyExpr expr)

-- tests -----------------------------------------------

runTC :: IOTypeError TType -> IO (Either TypeError TType)
runTC action = do
    r <- runErrorT action
    return r

main :: IO ()
main = do
    let expr = parse parseAnyExpr "tc test" "(lambda (x : int) (+ y 1))"
    env <- newTypedEnv
    addGlobalBinding env ("y", IntTy)
    case expr of
        Left err -> putStrLn (show err)
        Right e -> do
          r <- runTC $ typeOf env e
          putStrLn $ show r
          return ()








