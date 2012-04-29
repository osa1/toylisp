module Eval where

--import Control.Monad.Error

import Control.Monad (liftM)
import Data.IORef
import Control.Monad.IO.Class (liftIO)

import Error
import Types
import Parser

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [ ("car", car)
             , ("cdr", cdr)
             , ("cons", cons)
             , ("eqv?", eqv)

             , ("+", numericBinop (+))
             , ("-", numericBinop (-))
             , ("*", numericBinop (*))
             , ("/", numericBinop div)
             , ("mod", numericBinop mod)
             , ("quotient", numericBinop quot)
             , ("remainder", numericBinop rem)

             , ("=", numBoolBinop (==))
             , ("<", numBoolBinop (<))
             , (">", numBoolBinop (>))
             , ("/=", numBoolBinop (/=))
             , (">=", numBoolBinop (>=))
             , ("<=", numBoolBinop (<=))
             , ("&&", boolBoolBinop (&&))
             , ("||", boolBoolBinop (||))
             , ("string=?", strBoolBinop (==))
             , ("string<?", strBoolBinop (<))
             , ("string>?", strBoolBinop (>))
             , ("string<=?", strBoolBinop (<=))
             , ("string>=?", strBoolBinop (>=))

             , ("string?", stringp)
             , ("symbol?", symbolp)
             , ("number?", numberp)
             , ("list?", listp)
             , ("boolean?", boolp)
             , ("symbol->string", symbolToString)
             ]

stringp :: [LispVal] -> ThrowsError LispVal
stringp [(String _)] = return $ Bool True
stringp _ = return $ Bool False

symbolp :: [LispVal] -> ThrowsError LispVal
symbolp [(Atom _)] = return $ Bool True
symbolp _ = return $ Bool False

numberp :: [LispVal] -> ThrowsError LispVal
numberp [(Number _)] = return $ Bool True
numberp [(Float _)]  = return $ Bool True
numberp _ = return $ Bool False

listp :: [LispVal] -> ThrowsError LispVal
listp [(List _)] = return $ Bool True
listp _ = return $ Bool False

boolp :: [LispVal] -> ThrowsError LispVal
boolp [(Bool _)] = return $ Bool True
boolp _ = return $ Bool False

symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString [(Atom s)] = return $ String s
symbolToString [notSymbol] = throwError $ TypeMismatch "symbol" notSymbol

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                                then throwError $ NumArgs 2 args
                                else do left <- unpacker $ args !! 0
                                        right <- unpacker $ args !! 1
                                        return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr ::[LispVal] -> ThrowsError LispVal
cdr [List (x:xs)] = return $ List xs
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [DottedList [xs] x] = return x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ [x] ++ xs
cons [x, DottedList xs xlast] = return $ DottedList ([x] ++ xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1) , (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && (and $ map eqvPair $ zip arg1 arg2)
  where eqvPair (x1, x2) = case eqv [x1, x2] of
                                Left err -> False
                                Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    case lookup var env of
        Just v  -> liftIO $ readIORef v
        Nothing -> throwError $ UnboundVar "Getting an unbound variable" var

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var val = do
    env <- liftIO $ readIORef envRef
    case lookup var env of
        Just v  -> liftIO $ writeIORef v val
        Nothing -> throwError $ UnboundVar "Setting an unbound variable." var
    return val

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var val = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
        then setVar envRef var val >> return val
        else liftIO $ do
            valRef <- newIORef val
            env <- readIORef envRef
            writeIORef envRef ((var, valRef):env)
            return val

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = do
    env <- readIORef envRef
    extEnv <- extendEnv bindings env
    newIORef extEnv
  where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
        addBinding (var, val) = do
            ref <- newIORef val
            return (var, ref)

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Bool _)   = return val
eval _ (List [Atom "quote", val]) = return val

eval env (List [Atom "if", pred, conseq, alt]) = do
    result <- eval env pred
    case result of
        Bool False -> eval env alt
        _ -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var
eval env (List (Atom func:args)) = mapM (eval env) args >>= liftThrows . apply func
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

--main = do
--    r <- getLine
--    evaled <- return $ liftM show $ readExpr r >>= eval
--    putStrLn $ extractValue $ trapError evaled
--    main


