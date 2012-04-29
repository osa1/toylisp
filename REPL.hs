module REPL where

import Control.Monad
import System.Environment (getArgs)

import IO hiding (try)
import Eval
import Error
import Types
import Parser


flushStr :: String -> IO ()
flushStr str = do
    putStr str
    hFlush stdout
--flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = do
    flushStr prompt
    getLine
--readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr =
    runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

runOne :: String -> IO ()
runOne expr = nullEnv >>= flip evalAndPrint expr

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    result <- prompt
    if pred result
        then return ()
        else action result >> until_ pred prompt action

runRepl :: IO ()
--runRepl = until_ (== "quit") (readPrompt "λ> ") evalAndPrint
runRepl = nullEnv >>= until_ (== "quit") (readPrompt "λ> ") . evalAndPrint

main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> runRepl
        1 -> runOne $ args !! 0
        _ -> putStrLn "wrong num of args"