{-# OPTIONS_GHC -Wall -fno-warn-hi-shadowing -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}
module REPL where

import Control.Monad
import System.Environment (getArgs)

import System.IO
import Eval
--import Env
import Types
import Parser
import Prim
import Control.Monad.IO.Class (liftIO)

import System.Console.Haskeline

flushStr :: String -> IO ()
flushStr str = do
    putStr str
    hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = do
    flushStr prompt
    getLine

evalString :: Env -> String -> IO String
evalString env expr =
    runIOThrows $ liftM show  $ readExpr expr >>= (\expr -> eval' env expr EndCont)

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

--runOne :: [String] -> IO ()
--runOne args = do
--    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
--    r <- runIOThrows $ liftM show $ eval env (List [Symbol "load", String (head args)]) EndCont
--    hPutStrLn stderr r

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    result <- prompt
    unless (pred result) $ action result >> until_ pred prompt action

completeFun :: Monad m => String -> m [Completion]
completeFun s = return $ map simpleCompletion (prims ++ specials)

  where prims = map fst $ filter (\(n,_) -> take (length s) n == s) primitives
        specials = [] -- TODO

wordComplete :: Monad m => CompletionFunc m
wordComplete = completeWord Nothing " \t()\"\'#%" completeFun

runREPL :: IO ()
runREPL = do
    args <- getArgs
    env <- primitiveBindings
    if null args then
        runInputT (setComplete wordComplete defaultSettings) (loop env)
    else
        return ()
  where loop :: Env -> InputT IO ()
        loop env = do
          minput <- getInputLine "λ> "
          case minput of
              Nothing -> return ()
              Just "quit" -> return ()
              Just input -> do liftIO $ evalAndPrint env input
                               loop env