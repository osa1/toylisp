{-# LANGUAGE GADTs, NamedFieldPuns #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns
                -fwarn-unused-binds
                -fwarn-unused-matches #-}
module Eval where

import Control.Monad (liftM)
import Control.Monad.Error (throwError)
import Data.IORef
import Data.Maybe (isNothing)
import Control.Monad.IO.Class (liftIO)
import System.IO

import Types
import Env
import IO
import Parser

--primitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
--primitives = [ ("car", car)
--             , ("cdr", cdr)
--             , ("cons", cons)
--             , ("eqv?", eqv)

--             , ("+", numericBinop (+))
--             , ("-", numericBinop (-))
--             , ("*", numericBinop (*))
--             , ("/", numericBinop div)
--             , ("mod", numericBinop mod)
--             , ("quotient", numericBinop quot)
--             , ("remainder", numericBinop rem)

--             , ("=", numBoolBinop (==))
--             , ("<", numBoolBinop (<))
--             , (">", numBoolBinop (>))
--             , ("/=", numBoolBinop (/=))
--             , (">=", numBoolBinop (>=))
--             , ("<=", numBoolBinop (<=))
--             , ("&&", boolBoolBinop (&&))
--             , ("||", boolBoolBinop (||))
--             , ("string=?", strBoolBinop (==))
--             , ("string<?", strBoolBinop (<))
--             , ("string>?", strBoolBinop (>))
--             , ("string<=?", strBoolBinop (<=))
--             , ("string>=?", strBoolBinop (>=))

--             , ("string?", stringp)
--             , ("symbol?", symbolp)
--             , ("number?", numberp)
--             , ("list?", listp)
--             , ("boolean?", boolp)
--             , ("symbol->string", symbolToString)
--             -- , ("apply", applyProc)

--             -- IO functions
--             , ("open-input-file", makePort ReadMode)
--             , ("open-output-file", makePort WriteMode)
--             , ("close-input-port", closePort)
--             , ("close-output-port", closePort)
--             , ("read", readProc)
--             , ("write", writeProc)
--             , ("read-contents", readContents)
--             , ("read-all", readAll)

--             -- read
--             , ("read-form", lispRead)
--             ]


errorOrFalse :: Int -> [TVal] -> IOThrowsError TVal
errorOrFalse n vals = if length vals /= n then
                          throwError $ NumArgs n (length vals)
                      else
                          return $ Bool False

-- is there a way to generate this repetitive code?
stringp :: SimpleFunc
stringp [String _] = return $ Bool True
stringp args = errorOrFalse 1 args

symbolp :: SimpleFunc
symbolp [TSymbol _] = return $ Bool True
symbolp args = errorOrFalse 1 args

numberp :: SimpleFunc
numberp [Int _] = return $ Bool True
numberp [Float _] = return $ Bool True
numberp args = errorOrFalse 1 args

listp :: SimpleFunc
listp [TList _] = return $ Bool True
listp args = errorOrFalse 1 args

boolp :: SimpleFunc
boolp [Bool _] = return $ Bool True
boolp args = errorOrFalse 1 args

typeOfFun :: SimpleFunc
typeOfFun [val] = return $ TSymbol (show $ typeOf val)
typeOfFun args = throwError $ NumArgs 1 (length args)

symbolToString :: SimpleFunc
symbolToString [TSymbol s] = return $ String s
symbolToString [notSymbol] = throwError $ TypeMismatch SymbolType (show $ typeOf notSymbol)
symbolToString args = throwError $ NumArgs 1 (length args)

--numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> IOThrowsError LispVal
--numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
--numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

--boolBinop :: (LispVal -> IOThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> IOThrowsError LispVal
--boolBinop unpacker op args = if length args /= 2
--                                then throwError $ NumArgs 2 args
--                                else do left <- unpacker $ head args
--                                        right <- unpacker $ args !! 1
--                                        return $ Bool $ left `op` right

--numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> IOThrowsError LispVal
--numBoolBinop = boolBinop unpackNum

--strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> IOThrowsError LispVal
--strBoolBinop = boolBinop unpackStr

--boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> IOThrowsError LispVal
--boolBoolBinop = boolBinop unpackBool

--unpackStr :: LispVal -> IOThrowsError String
--unpackStr (String s) = return s
--unpackStr notString = throwError $ TypeMismatch "string" notString

--unpackBool :: LispVal -> IOThrowsError Bool
--unpackBool (Bool b) = return b
--unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

--unpackNum :: LispVal -> IOThrowsError Integer
--unpackNum (Number n) = return n
--unpackNum (List [n]) = unpackNum n
--unpackNum notNum = throwError $ TypeMismatch "number" notNum

--car :: [LispVal] -> IOThrowsError LispVal
--car [List (x:__)] = return x
--car [DottedList (x:_) _] = return x
--car [badArg] = throwError $ TypeMismatch "pair" badArg
--car badArgList = throwError $ NumArgs 1 badArgList

--cdr ::[LispVal] -> IOThrowsError LispVal
--cdr [List (_:xs)] = return $ List xs
--cdr [DottedList (_:fx:xs) x] = return $ DottedList (fx:xs) x
--cdr [DottedList [_] x] = return x
--cdr [badArg] = throwError $ TypeMismatch "pair" badArg
--cdr badArgList = throwError $ NumArgs 1 badArgList

--cons :: [LispVal] -> IOThrowsError LispVal
--cons [x1, List []] = return $ List [x1]
--cons [x, List xs] = return $ List $ x : xs
--cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
--cons [x1, x2] = return $ DottedList [x1] x2
--cons badArgList = throwError $ NumArgs 2 badArgList

--eqv :: [LispVal] -> IOThrowsError LispVal
--eqv vals = case eqv' vals of
--    Left err -> throwError err
--    Right val -> return val
--eqv' :: [LispVal] -> Either LispError LispVal
--eqv' [Bool arg1, Bool arg2] = return $ Bool $ arg1 == arg2
--eqv' [Number arg1, Number arg2] = return $ Bool $ arg1 == arg2
--eqv' [String arg1, String arg2] = return $ Bool $ arg1 == arg2
--eqv' [Atom arg1, Atom arg2] = return $ Bool $ arg1 == arg2
--eqv' [DottedList xs x, DottedList ys y] = eqv' [List $ xs ++ [x], List $ ys ++ [y]]
--eqv' [List arg1, List arg2] = return $ Bool $ (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)
--  where eqvPair (x1, x2) = case eqv' [x1, x2] of
--                                Left _ -> False
--                                Right (Bool val) -> val
--eqv' [_, _] = return $ Bool False
--eqv' badArgList = throwError $ NumArgs 2 badArgList

--makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
--makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode
--makePort _ [x] = throwError $ TypeMismatch "string" x
--makePort _ args = throwError $ NumArgs 1 args

--closePort :: [LispVal] -> IOThrowsError LispVal
--closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
--closePort [x] = throwError $ TypeMismatch "port" x
--closePort args = throwError $ NumArgs 1 args

--readProc :: [LispVal] -> IOThrowsError LispVal
--readProc [] = readProc [Port stdin]
--readProc [Port port] = liftIO (hGetLine port) >>= readExpr
--readProc [x] = throwError $ TypeMismatch "port" x
--readProc args = throwError $ NumArgs 1 args


--writeProc :: [LispVal] -> IOThrowsError LispVal
--writeProc [obj] = writeProc [obj, Port stdout]
--writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)
--writeProc args@(_:_:_) = throwError $ NumArgs 2 args
--writeProc [] = throwError $ NumArgs 2 []

--readContents :: [LispVal] -> IOThrowsError LispVal
--readContents [String filename] = liftM String $ liftIO $ readFile filename
--readContents [x] = throwError $ TypeMismatch "string" x
--readContents args@(_:_) = throwError $ NumArgs 1 args
--readContents [] = throwError $ NumArgs 1 []

--load :: String -> IOThrowsError [LispVal]
--load filename = liftIO (readFile filename) >>= readExprList

--readAll :: [LispVal] -> IOThrowsError LispVal
--readAll [String filename] = liftM List $ load filename
--readAll [x] = throwError $ TypeMismatch "string" x
--readAll args@(_:_) = throwError $ NumArgs 1 args
--readAll [] = throwError $ NumArgs 1 []

--lispRead :: [LispVal] -> IOThrowsError LispVal
--lispRead [String form] = readOrThrow parseExpr form
--lispRead [x] = throwError $ TypeMismatch "string" x
--lispRead args = throwError $ NumArgs 1 args

--primitiveBindings :: IO Env
--primitiveBindings =
--    let makeFunc constructor (var, func) = (var, constructor func)
--        addPrimitives = flip bindVars $ map (makeFunc PrimitiveFunc) primitives
--    in nullEnv >>= addPrimitives

eval' :: Env -> AnyExpr -> Cont -> IOThrowsError TVal
eval' env (AnyExpr expr) = eval env expr


eval :: Env -> Expr a -> Cont -> IOThrowsError TVal
eval env (Symbol s) cont = getVar env s >>= applyCont cont
eval env (Lambda params body) cont = makeNormalFunc env params body >>= applyCont cont

-- Function application
eval env (Application (Left (Symbol fun)) params) cont =
    getVar env fun >>= applyCont (ApplyCont params [] env cont)
eval env (Application (Right lambda) params) cont =
    eval env lambda (ApplyCont params [] env cont)

eval env (Define (Symbol name) body) cont = eval' env body (DefineCont name env cont)
eval env (Set (Symbol name) body) cont = eval' env body (SetCont name env cont)

eval env (If pred thenE elseE) cont = eval' env pred (PredCont thenE elseE env cont)

-- TODO: fexprs
eval _ (Fexpr _ _) _ = do
    liftIO $ putStrLn "Fexprs are not yet implemented."
    undefined

eval _ (Val v) cont = applyCont cont v
eval _ (List _) _ = do
    liftIO $ putStrLn "Lists are not yet implemented."
    undefined -- TODO: why did I add this?

eval _ (EvalExp _) _ = do
    liftIO $ putStrLn "Eval function is not yet implemented."
    undefined -- TODO: ???

eval env (CallCC (Left (Symbol fun))) cont =
    getVar env fun >>= applyCont (ApplyCont [] [Continuation cont] env cont)
eval env (CallCC (Right lambda)) cont =
    eval env lambda (ApplyCont [] [Continuation cont] env cont)

apply :: TVal -> [TVal] -> Cont -> IOThrowsError TVal
apply (SimpleFunc fun) args cont = fun args >>= applyCont cont

apply (Func params varargs body closure) args cont =
    if num params /= num args && isNothing varargs
        then throwError $ NumArgs (length params) (length args)
        else liftIO (bindVars closure $ zip (map (\(Symbol s) -> s) params) args) >>=
            bindVarArgs varargs >>= evalBody
  where remainingArgs :: [TVal]
        remainingArgs = drop (length params) args

        num :: [a] -> Integer
        num = toInteger . length

        bindVarArgs :: Maybe (Expr Symbol) -> Env -> IOThrowsError Env
        bindVarArgs args env = case args of
            Just (Symbol argName) -> liftIO $ bindVars env [(argName, TList remainingArgs)]
            Nothing -> return env

        evalBody :: Env -> IOThrowsError TVal
        evalBody env = applyCont (SeqLastCont body Nothing env cont) Nil
        -- FIXME: maybe I should completely remove last parameter of applyCont

apply (Continuation c) [param] _ = applyCont c param

apply wtf _ _ = throwError $ TypeMismatch FunctionType (show $ typeOf wtf)


applyCont :: Cont -> TVal -> IOThrowsError TVal
applyCont EndCont val = return val
applyCont (PredCont _ elseE env cont) (Bool False) = eval' env elseE cont
applyCont (PredCont thenE _ env cont) _ = eval' env thenE cont

applyCont (ApplyCont (x:xs) args env cont) fun = do
    r <- eval' env x EndCont
    applyCont (ApplyCont xs (args++[r]) env cont) fun
-- TODO: be sure this works as expected
applyCont (ApplyCont [] args _ cont) fun = apply fun args cont

applyCont (DefineCont name env cont) val = defineVar env name val >>= applyCont cont
applyCont (SetCont name env cont) val = setVar env name val >>= applyCont cont

applyCont (SeqLastCont (x:xs) _ env cont) _ = do
    r <- eval' env x EndCont
    applyCont (SeqLastCont xs (Just r) env cont) Nil

applyCont (SeqLastCont [] (Just v) _ cont) _ = applyCont cont v
applyCont (SeqLastCont [] Nothing _ cont) _ = applyCont cont Nil


--applyCont :: Cont -> LispVal -> IOThrowsError LispVal
--applyCont EndCont val = return val
--applyCont (PredCont conseq alt env cont) val = evalCPS env val (TestCont conseq alt env cont)
--applyCont (TestCont conseq _ env cont) (Bool True) = evalCPS env conseq cont
--applyCont (TestCont _ alt env cont) (Bool False) = evalCPS env alt cont
--applyCont TestCont{} notBool = throwError $ TypeMismatch "bool" notBool
--applyCont (SetCont var env cont) form = setVar env var form >>= applyCont cont

--applyCont (SeqCont (x:xs) vals env cont) fun = do
--    r <- evalCPS env x EndCont
--    applyCont (SeqCont xs (vals++[r]) env cont) fun
--applyCont (SeqCont [] vals _ cont) fun = applyCPS fun vals cont

--applyCont (DefineCont var env cont) form = defineVar env var form >>= applyCont cont

--applyCont (SeqLastCont (x:xs) env cont) expr = do
--    evalCPS env expr EndCont
--    applyCont (SeqLastCont xs env cont) x
--applyCont (SeqLastCont [] env cont) expr = evalCPS env expr cont

--applyCont _ _ = undefined

