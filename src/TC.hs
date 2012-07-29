{-# OPTIONS_GHC -Wall
                -fno-warn-missing-signatures
                -fno-warn-hi-shadowing
                -fno-warn-unused-do-bind
                -fno-warn-name-shadowing #-}
{-# LANGUAGE GADTs #-}

module TC where

import Prelude hiding (lookup)
import Types
import qualified Data.Map as M
import Data.IORef (readIORef, modifyIORef, newIORef, writeIORef)
import Control.Monad (liftM)

import Parser
import Text.ParserCombinators.Parsec

type BindingError = String

newNamedEnv :: IO NamedEnv
newNamedEnv = do
    -- FIXME: this code stucks in an infinite loop
    --vals <- mapM (\_ -> newIORef Nil) [1..] -- TODO: find idiomatic way
    --env <- newIORef $ M.fromList (zip ["+"] vals)
    nil <- newIORef Nil
    env <- newIORef $ M.fromList [("+", nil)]
    return (env, [])

lookup :: NamedEnv -> String -> IO Bool
lookup (globalenv, scope) s = do
    globenv <- readIORef globalenv
    case M.lookup s globenv of
        Just _ -> return True
        Nothing -> if any (\frame -> any (\(name,_) -> name == s) frame) scope then
                       return True
                   else
                       return False

addLocalBindings :: NamedEnv -> [String] -> NamedEnv
addLocalBindings (global, locals) names = (global, map (\s -> (s, Nil)) names : locals)

addGlobalBinding :: NamedEnv -> String -> IO ()
addGlobalBinding (globalEnv,_) name = do
    env <- readIORef globalEnv
    val <- newIORef Nil
    writeIORef globalEnv (M.insert name val env)

checkBindings :: NamedEnv -> AnyExpr -> IO (Maybe BindingError)
checkBindings env (AnyExpr (Symbol s)) = do
    r <- lookup env s
    if r then return Nothing else return $ Just s
checkBindings env (AnyExpr (Lambda params body)) = do
    let env' = addLocalBindings env (map (\(Symbol s) -> s) params)
    checkBindingsSeq env' body
checkBindings env (AnyExpr (Application x xs)) = checkBindingsSeq env (x:xs)
checkBindings env (AnyExpr (If guard thenE elseE)) = checkBindingsSeq env [guard,thenE,elseE]
checkBindings _   (AnyExpr Val{}) = return Nothing
checkBindings env (AnyExpr (List exprs)) = checkBindingsSeq env exprs
checkBindings env (AnyExpr (Define (Symbol name) body)) = addGlobalBinding env name >> checkBindings env body
checkBindings env (AnyExpr (Set name body)) = checkBindings env (AnyExpr name) >> checkBindings env body

checkBindingsSeq :: NamedEnv -> [AnyExpr] -> IO (Maybe BindingError)
checkBindingsSeq _ [] = return Nothing
checkBindingsSeq env (x:xs) = do
    r <- checkBindings env x
    case r of
        Just e -> return (Just e)
        Nothing -> checkBindingsSeq env xs

-- tests -----------------------------------------------

main :: IO ()
main = do
    let expr = parse parseAnyExpr "tc test" "(lambda (x) (+ y 1))"
    env <- newNamedEnv
    --addGlobalBinding env "y"
    case expr of
        Left err -> putStrLn (show err)
        Right e -> do
          r <- checkBindings env e
          putStrLn (show r)







