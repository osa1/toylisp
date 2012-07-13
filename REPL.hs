module REPL where

import Control.Monad
import System.Environment (getArgs)

import System.IO
import Eval
import Env
import Types
import Parser


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

runRepl :: IO ()
runRepl = do
    env <- primitiveBindings
    evalString env "(load \"stdlib.scm\")"
    until_ (== "quit") (readPrompt "λ> ") (evalAndPrint env)



main :: IO ()
main = do
    args <- getArgs
    if null args
        then runRepl
        --else runOne args
        else return ()
