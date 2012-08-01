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
    typeOf env (TList vals) = do
        e <- typeOf env (head vals)
        return $ LstTy e
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

unpackSymbol :: Expr Symbol -> String
unpackSymbol (Symbol s) = s

collectDefTypes :: TypedEnv -> [AnyExpr] -> IOTypeError ()
collectDefTypes env (def@(AnyExpr (Define (Symbol name) _)):defs) = do
    ty <- typeOf env def
    liftIO $ addGlobalBinding env (name, ty)
    collectDefTypes env defs
collectDefTypes env (_:defs) = collectDefTypes env defs
collectDefTypes _ [] = return ()

checkExprs :: TypedEnv -> [AnyExpr] -> IOTypeError ()
checkExprs env@(global,_) ((AnyExpr (Define (Symbol name) def)):defs) = do
    genv <- liftIO $ readIORef global
    let ty = M.lookup name genv
    case ty of
        Nothing -> throwError $ "name is not in global env: " ++ name
        Just ty' -> do
            ty'' <- typeOf env def
            liftIO $ putStrLn $ "type in glob env: " ++ show ty'
            liftIO $ putStrLn $ "checked type: " ++ show ty''
            if ty' == ty'' then do
                liftIO $ putStrLn "ok"
                checkExprs env defs
            else throwError $ "type error: " ++ name
checkExprs env (expr:exprs) = do
    typeOf env expr
    checkExprs env exprs
checkExprs _ [] = return ()

instance Typed AnyExpr where
  typeOf env (AnyExpr (Symbol s)) = TC.lookup env s
  typeOf env (AnyExpr (Lambda params ret body)) = do
      let pts = map (\(s,t) -> (unpackSymbol s, t)) params
      let env' = addLocalBindings env pts
      ret' <- liftM last $ checkSeq env' body
      if ret' == ret then return (FuncTy pts ret')  else throwError "type mismatch on function return value"
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
  typeOf env (AnyExpr (Define (Symbol name) (AnyExpr (Lambda params ret _)))) = do
      -- XXX: this part is strange, it doesn't really type check. it just returns declared
      -- type of the function. The point of this is to collect type signatures before
      -- moving to type-checking.
      let ty = FuncTy (map (\(s, t) -> (unpackSymbol s, t)) params) ret
      liftIO $ addGlobalBinding env (name, ty)
      return ty
  typeOf env (AnyExpr (Define (Symbol name) body)) = do
      ty <- typeOf env body
      liftIO $ addGlobalBinding env (name, ty)
      return ty
  typeOf _ expr = throwError $ "not yet implemented: " ++ show expr

instance Typed (Expr a) where
    typeOf env expr = typeOf env (AnyExpr expr)

-- tests -----------------------------------------------

runTC :: IOTypeError a -> IO (Either TypeError a)
runTC = runErrorT

main :: IO ()
main = do
    let text = "(defun f2 (y : bool x : bool) : int 1)(defun f1 (x : int) : bool 1) "
    let exprs = parse (many1 parseAnyExpr) "tc test" text
    env <- newTypedEnv
    case exprs of
        Left err -> putStrLn (show err)
        Right exprs' -> do
            runTC $ collectDefTypes env exprs'
            let (genv, _) = env
            globals <- readIORef genv
            putStrLn $ show (M.toList globals)
            r <- runTC $ checkExprs env exprs'
            case r of
                Left err -> putStrLn $ show err
                Right _ -> putStrLn "type check OK"

{-main :: IO ()
main = do
    [>let expr = parse parseAnyExpr "tc test" "(lambda (x : int) : int (+ y 1))"<]
    [>let expr = parse parseAnyExpr "tc test" "(defun x (a : int b : bool) : bool (+ 1 2))"<]
    let expr = parse parseAnyExpr "tc test" "(lambda (a : int b : bool) : bool (+ 1 2))"
    env <- newTypedEnv
    addGlobalBinding env ("y", IntTy)
    case expr of
        Left err -> putStrLn (show err)
        Right e -> do
          r <- runTC $ typeOf env e
          putStrLn $ show r
          return ()-}
