{-# OPTIONS_GHC -fwarn-incomplete-patterns
                -fwarn-unused-binds
                -fwarn-unused-matches #-}
module Eval
    (
      evalCPS
    , primitiveBindings
    , bindVars
    ) where

import Control.Monad (liftM)
import Data.IORef
import Control.Monad.IO.Class (liftIO)
import System.IO

import Types
import Parser

primitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
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
             -- , ("apply", applyProc)

             -- IO functions
             , ("open-input-file", makePort ReadMode)
             , ("open-output-file", makePort WriteMode)
             , ("close-input-port", closePort)
             , ("close-output-port", closePort)
             , ("read", readProc)
             , ("write", writeProc)
             , ("read-contents", readContents)
             , ("read-all", readAll)

             -- read
             , ("read-form", lispRead)
             ]

stringp :: [LispVal] -> IOThrowsError LispVal
stringp [String _] = return $ Bool True
stringp _ = return $ Bool False

symbolp :: [LispVal] -> IOThrowsError LispVal
symbolp [Atom _] = return $ Bool True
symbolp _ = return $ Bool False

numberp :: [LispVal] -> IOThrowsError LispVal
numberp [Number _] = return $ Bool True
numberp [Float _]  = return $ Bool True
numberp _ = return $ Bool False

listp :: [LispVal] -> IOThrowsError LispVal
listp [List _] = return $ Bool True
listp _ = return $ Bool False

boolp :: [LispVal] -> IOThrowsError LispVal
boolp [Bool _] = return $ Bool True
boolp _ = return $ Bool False

symbolToString :: [LispVal] -> IOThrowsError LispVal
symbolToString [Atom s] = return $ String s
symbolToString [notSymbol] = throwError $ TypeMismatch "symbol" notSymbol
symbolToString args@(_:xs) = throwError $ NumArgs (1 + length xs) args
symbolToString [] = throwError $ NumArgs 1 []

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> IOThrowsError LispVal
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> IOThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> IOThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                                then throwError $ NumArgs 2 args
                                else do left <- unpacker $ head args
                                        right <- unpacker $ args !! 1
                                        return $ Bool $ left `op` right

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> IOThrowsError LispVal
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> IOThrowsError LispVal
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> IOThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> IOThrowsError String
unpackStr (String s) = return s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> IOThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

unpackNum :: LispVal -> IOThrowsError Integer
unpackNum (Number n) = return n
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

car :: [LispVal] -> IOThrowsError LispVal
car [List (x:__)] = return x
car [DottedList (x:_) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr ::[LispVal] -> IOThrowsError LispVal
cdr [List (_:xs)] = return $ List xs
cdr [DottedList (_:fx:xs) x] = return $ DottedList (fx:xs) x
cdr [DottedList [_] x] = return x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> IOThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> IOThrowsError LispVal
eqv vals = case eqv' vals of
    Left err -> throwError err
    Right val -> return val
eqv' :: [LispVal] -> Either LispError LispVal
eqv' [Bool arg1, Bool arg2] = return $ Bool $ arg1 == arg2
eqv' [Number arg1, Number arg2] = return $ Bool $ arg1 == arg2
eqv' [String arg1, String arg2] = return $ Bool $ arg1 == arg2
eqv' [Atom arg1, Atom arg2] = return $ Bool $ arg1 == arg2
eqv' [DottedList xs x, DottedList ys y] = eqv' [List $ xs ++ [x], List $ ys ++ [y]]
eqv' [List arg1, List arg2] = return $ Bool $ (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)
  where eqvPair (x1, x2) = case eqv' [x1, x2] of
                                Left _ -> False
                                Right (Bool val) -> val
eqv' [_, _] = return $ Bool False
eqv' badArgList = throwError $ NumArgs 2 badArgList

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode
makePort _ [x] = throwError $ TypeMismatch "string" x
makePort _ args = throwError $ NumArgs 1 args

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort [x] = throwError $ TypeMismatch "port" x
closePort args = throwError $ NumArgs 1 args

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= readExpr
readProc [x] = throwError $ TypeMismatch "port" x
readProc args = throwError $ NumArgs 1 args


writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)
writeProc args@(_:_:_) = throwError $ NumArgs 2 args
writeProc [] = throwError $ NumArgs 2 []

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename
readContents [x] = throwError $ TypeMismatch "string" x
readContents args@(_:_) = throwError $ NumArgs 1 args
readContents [] = throwError $ NumArgs 1 []

load :: String -> IOThrowsError [LispVal]
load filename = liftIO (readFile filename) >>= readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename
readAll [x] = throwError $ TypeMismatch "string" x
readAll args@(_:_) = throwError $ NumArgs 1 args
readAll [] = throwError $ NumArgs 1 []

lispRead :: [LispVal] -> IOThrowsError LispVal
lispRead [String form] = readOrThrow parseExpr form
lispRead [x] = throwError $ TypeMismatch "string" x
lispRead args = throwError $ NumArgs 1 args

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

primitiveBindings :: IO Env
primitiveBindings =
    let makeFunc constructor (var, func) = (var, constructor func)
        addPrimitives = flip bindVars $ map (makeFunc PrimitiveFunc) primitives
    in nullEnv >>= addPrimitives

evalCPS :: Env -> LispVal -> Cont -> IOThrowsError LispVal
evalCPS _ val@(String _) cont = applyCont cont val
evalCPS _ val@(Number _) cont = applyCont cont val
evalCPS _ val@(Bool _) cont = applyCont cont val
evalCPS _ (List [Atom "quote", val]) cont = applyCont cont val
evalCPS env (Atom id) cont = getVar env id >>= applyCont cont
evalCPS env (List [Atom "if", pred, conseq, alt]) cont =
    applyCont (PredCont conseq alt env cont) pred
evalCPS env (List [Atom "set!", Atom var, form]) cont =
    evalCPS env form (SetCont var env cont)
evalCPS env (List [Atom "define", Atom var, form]) cont =
    evalCPS env form (DefineCont var env cont)
evalCPS env (List (Atom "define" : List (Atom var : params) : body)) cont =
    makeNormalFunc env params body >>= applyCont (DefineCont var env cont)
evalCPS env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) cont =
    makeVarargs varargs env params body >>= applyCont (DefineCont var env cont)

-- call/cc
evalCPS env (List [Atom "call/cc" , List (Atom "lambda" : List params : body)]) cont =
    makeNormalFunc env params body >>= \fun -> applyCPS fun [Continuation cont] cont

-- eval
evalCPS env (List [Atom "eval", (List [Atom "quote", rest])]) cont = evalCPS env rest cont
evalCPS env (List [Atom "eval", val]) cont = evalCPS env val cont

-- Lambda
evalCPS env (List (Atom "lambda" : List params : body)) cont =
    makeNormalFunc env params body >>= applyCont cont
evalCPS env (List (Atom "lambda" : varargs@(Atom _) : body)) cont =
    makeVarargs varargs env [] body >>= applyCont cont
evalCPS env (List (Atom "lambda" : DottedList params varargs : body)) cont =
    makeVarargs varargs env params body >>= applyCont cont

-- Load command
evalCPS env (List [Atom "load", String filename]) cont = do
    contents <- load filename
    applyCont (SeqLastCont (tail contents) env cont) (head contents)

-- Function application
evalCPS env (List (function : args)) cont =
    evalCPS env function (SeqCont args [] env cont)

evalCPS _ badForm _ = throwError $ BadSpecialForm "Unrecognized special form" badForm


applyCont :: Cont -> LispVal -> IOThrowsError LispVal
applyCont EndCont val = return val
applyCont (PredCont conseq alt env cont) val = evalCPS env val (TestCont conseq alt env cont)
applyCont (TestCont conseq _ env cont) (Bool True) = evalCPS env conseq cont
applyCont (TestCont _ alt env cont) (Bool False) = evalCPS env alt cont
applyCont TestCont{} notBool = throwError $ TypeMismatch "bool" notBool
applyCont (SetCont var env cont) form = setVar env var form >>= applyCont cont

applyCont (SeqCont (x:xs) vals env cont) fun = do
    r <- evalCPS env x EndCont
    applyCont (SeqCont xs (vals++[r]) env cont) fun
applyCont (SeqCont [] vals _ cont) fun = applyCPS fun vals cont

applyCont (DefineCont var env cont) form = defineVar env var form >>= applyCont cont

applyCont (SeqLastCont (x:xs) env cont) expr = applyCont (SeqLastCont xs env cont) x
applyCont (SeqLastCont [] env cont) expr = evalCPS env expr cont

applyCont _ _ = undefined

applyCPS :: LispVal -> [LispVal] -> Cont -> IOThrowsError LispVal
applyCPS (PrimitiveFunc func) args@(_:_) cont = func args >>= applyCont cont

applyCPS (Func params varargs body closure) args@(_:_) cont =
    if num params /= num args && varargs == Nothing
        then throwError $ NumArgs (length params) args
        else (liftIO $ bindVars closure $ zip params args) >>=
            bindVarArgs varargs >>= evalBody
  where remainingArgs = drop (length params) args
        num = toInteger . length
        bindVarArgs arg env = case arg of
            Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
            Nothing -> return env
        evalBody :: Env -> IOThrowsError LispVal
        evalBody env = applyCont (SeqLastCont (tail body) env cont) (head body)
applyCPS (Continuation c) [param] _ = applyCont c param

applyCPS func@PrimitiveFunc{} [] _ = throwError $ NumArgs 2 [func]
applyCPS func@Func{} [] _ = throwError $ NumArgs 2 [func] -- FIXME: first param of NumArgs
applyCPS func@Continuation{} [] _ = throwError $ NumArgs 1 [func]
applyCPS notFunc _ _ = throwError $ TypeMismatch "function" notFunc

