{-# OPTIONS_GHC -Wall -fno-warn-hi-shadowing -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}
module Env where

import Types

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Error (throwError)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import qualified Data.List as L
import qualified Data.Map as M

newEnv :: [(String, a)] -> Env a
newEnv bindings = (M.fromList bindings, [])

getVar :: Env a -> String -> IOThrowsError a
getVar env var = case Env.lookup env var of
    Just v -> return v
    Nothing -> throwError $ UnboundVar "Getting an unbound variable" var

getRef :: Env (IORef a) -> String -> IOThrowsError a
getRef env var = do
    ref <- getVar env var
    liftIO (readIORef ref)

lookup :: Env a -> String -> Maybe a
lookup (globenv, scope) s =
    case searchScope scope s of
        Just t -> return t
        Nothing -> M.lookup s globenv
  where searchScope :: [[(String, a)]] -> String -> Maybe a
        searchScope [] _ = Nothing
        searchScope (x:xs) name = case L.lookup name x of
                                      Nothing -> searchScope xs name
                                      Just t -> return t

addLocalBindings :: Env a -> [(String, a)] -> Env a
addLocalBindings (global,scope) bindings = (global, bindings:scope)

addLocalRefs :: Env (IORef a) -> [(String, a)] -> IO (Env (IORef a))
addLocalRefs env bindings = do
    refs <- makeRefs bindings
    return $ addLocalBindings env refs
  where makeRefs :: [(String, a)] -> IO [(String, IORef a)]
        makeRefs [] = return []
        makeRefs ((n,v):xs) = do
            ref <- newIORef v
            rest <- liftIO $ makeRefs xs
            return $ (n, ref) : rest

addGlobalBinding :: Env a -> (String, a) -> Env a
addGlobalBinding (global,scope) (name,ty) = (M.insert name ty global, scope)

addGlobalRef :: Env (IORef a) -> (String, a) -> IO (Env (IORef a))
addGlobalRef env (name,ty) = do
    ref <- newIORef ty
    return $ addGlobalBinding env (name,ref)

setVar :: Env (IORef a) -> String -> a -> IOThrowsError (IORef a)
setVar env var val = do
    case Env.lookup env var of
        Just ref -> liftIO (modifyIORef ref (const val)) >> return ref
        Nothing -> throwError $ UnboundVar "Setting an unbound variable." var

defineVar :: Env (IORef a) -> String -> a -> IOThrowsError (Env (IORef a))
defineVar env var val = do
    case Env.lookup env var of
        Just ref -> liftIO $ modifyIORef ref (const val) >> return env
        Nothing -> liftIO $ do
            valRef <- newIORef val
            return $ addGlobalBinding env (var, valRef)
