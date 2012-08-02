{-# OPTIONS_GHC -Wall
                -fno-warn-missing-signatures
                -fno-warn-hi-shadowing
                -fno-warn-unused-do-bind
                -fno-warn-name-shadowing #-}
{-# LANGUAGE GADTs, NamedFieldPuns #-}

module TC where

import Types
import Env
import qualified Data.Map as M
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

newTypedEnv :: TypedEnv
newTypedEnv = (M.fromList [("+", FuncTy [("i1", IntTy), ("i2", IntTy)] IntTy)], [])

checkSeq :: Typed a => TypedEnv -> [a] -> ErrorT TypeError IO [TType]
checkSeq env exprs = sequence $ map (typeOf env) exprs

unpackSymbol :: Expr Symbol -> String
unpackSymbol (Symbol s) = s

collectDefTypes :: TypedEnv -> [AnyExpr] -> IOTypeError TypedEnv
collectDefTypes env (def@(AnyExpr (Define (Symbol name) _)):defs) = do
    ty <- typeOf env def
    collectDefTypes (addGlobalBinding env (name,ty)) defs
collectDefTypes env (_:defs) = collectDefTypes env defs
collectDefTypes env [] = return env

checkExprs :: TypedEnv -> [AnyExpr] -> IOTypeError ()
checkExprs env@(genv,_) ((AnyExpr (Define (Symbol name) def)):defs) = do
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
  typeOf env (AnyExpr (Symbol s)) = case Env.lookup env s of
      Nothing -> throwError $ "unbound var " ++ s
      Just t -> return t
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
  typeOf _ (AnyExpr (Define _ (AnyExpr (Lambda params ret _)))) =
      -- XXX: this part is strange, it doesn't really type check. it just returns declared
      -- type of the function. The point of this is to collect type signatures before
      -- moving to type-checking.
      return $ FuncTy (map (\(s, t) -> (unpackSymbol s, t)) params) ret
  typeOf env (AnyExpr (Define _ body)) = typeOf env body
  typeOf _ expr = throwError $ "not yet implemented: " ++ show expr

instance Typed (Expr a) where
    typeOf env expr = typeOf env (AnyExpr expr)

-- tests -----------------------------------------------

runTC :: IOTypeError a -> IO (Either TypeError a)
runTC = runErrorT

main :: IO ()
main = do
    --let text = "(defun f2 (y : bool x : bool) : int 1)(defun f1 (x : int) : bool #t) "
    --let text = "((lambda (x : int) : int ((lambda (x : bool) : int 10) #f)) 10)"
    let text = "(defun f2 () : int (f1)) (defun f1 () : int (f2))"
    let exprs = parse (many1 parseAnyExpr) "tc test" text
    let env = newTypedEnv
    case exprs of
        Left err -> putStrLn (show err)
        Right exprs' -> do
            env' <- runTC $ collectDefTypes env exprs'
            case env' of
                Left err -> putStrLn $ show err
                Right genv -> do
                  r <- runTC $ checkExprs genv exprs'
                  case r of
                      Left err -> putStrLn $ show err
                      Right _ -> putStrLn "type check OK"
