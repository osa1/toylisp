module REPL where

import Control.Monad
import System.Environment (getArgs)

import IO hiding (try)
import Eval
import Error
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

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn


until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    result <- prompt
    if pred result
        then return ()
        else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "λ> ") evalAndPrint

main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> runRepl
        1 -> evalAndPrint $ args !! 0
        _ -> putStrLn "wrong num of args"