module Env where

import Types

import Data.IORef (writeIORef, readIORef, newIORef)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (liftM)
import Control.Monad.Error (throwError)

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError TVal
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    case lookup var env of
        Just v  -> liftIO $ readIORef v
        Nothing -> throwError $ UnboundVar "Getting an unbound variable" var

setVar :: Env -> String -> TVal -> IOThrowsError TVal
setVar envRef var val = do
    env <- liftIO $ readIORef envRef
    case lookup var env of
        Just v  -> liftIO $ writeIORef v val
        Nothing -> throwError $ UnboundVar "Setting an unbound variable." var
    return val

defineVar :: Env -> String -> TVal -> IOThrowsError TVal
defineVar envRef var val = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
        then setVar envRef var val >> return val
        else liftIO $ do
            valRef <- newIORef val
            env <- readIORef envRef
            writeIORef envRef ((var, valRef):env)
            return val

bindVars :: Env -> [(String, TVal)] -> IO Env
bindVars envRef bindings = do
    env <- readIORef envRef
    extEnv <- extendEnv bindings env
    newIORef extEnv
  where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
        addBinding (var, val) = do
            ref <- newIORef val
            return (var, ref)
