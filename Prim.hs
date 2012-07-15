{-# OPTIONS_GHC -Wall -fno-warn-hi-shadowing -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}
{-# LANGUAGE GADTs #-}
module Prim
    ( primitives
    , primitiveBindings
    ) where

import Types
import Control.Monad.Error (throwError)
import IO
import Parser (readAsSyntax)
import Env
import Eval (eval)
import Eq ()

primitives :: [(String, PrimFunc)]
primitives = [ ("first", first)
             , ("rest", rest)
             , ("cons", cons)
             , ("eq", eq)

             , ("+", numericBinop (+))
             , ("-", numericBinop (-))
             , ("*", numericBinop (*))
             , ("div", numericBinop div)
             , ("mod", numericBinop mod)
             , ("quot", numericBinop quot)
             , ("rem", numericBinop rem)

             , ("=", numBoolBinop (==))
             , ("<", numBoolBinop (<))
             , (">", numBoolBinop (>))
             , ("/=", numBoolBinop (/=))
             , (">=", numBoolBinop (>=))
             , ("<=", numBoolBinop (<=))
             , ("&&", boolBoolBinop (&&))
             , ("||", boolBoolBinop (||))

             --, ("string=?", strBoolBinop (==))
             --, ("string<?", strBoolBinop (<))
             --, ("string>?", strBoolBinop (>))
             --, ("string<=?", strBoolBinop (<=))
             --, ("string>=?", strBoolBinop (>=))

             , ("string?", stringp)
             , ("symbol?", symbolp)
             , ("number?", numberp)
             , ("list?", listp)
             , ("boolean?", boolp)
             , ("symbol->string", symbolToString)
             -- , ("apply", applyProc)

             , ("type-of", typeOfFun)

             ---- IO functions
             , ("println", println)
             --, ("open-input-file", makePort ReadMode)
             --, ("open-output-file", makePort WriteMode)
             --, ("close-input-port", closePort)
             --, ("close-output-port", closePort)
             --, ("read", readProc)
             --, ("write", writeProc)
             --, ("read-contents", readContents)
             --, ("read-all", readAll)

             -- read
             , ("read", readForm)

             -- eval
             --("eval", evalForm)
             ]

fexprs :: [(String, PrimFexpr)]
fexprs = [ ("apply", applyProc)
         , ("eval", evalProc)
         ]


errorOrFalse :: Int -> [TVal] -> IOThrowsError TVal
errorOrFalse n vals = if length vals /= n then
                          throwError $ NumArgs n (length vals)
                      else
                          return $ Bool False

-- is there a way to generate this repetitive code?
first :: PrimFunc
first [TList lst] = return $ head lst
first [notList] = throwError $ TypeMismatch ListType (typeOf notList)
first args = throwError $ NumArgs 1 (length args)

rest :: PrimFunc
rest [TList lst] = return $ TList (tail lst)
rest [notList] = throwError $ TypeMismatch ListType (typeOf notList)
rest args = throwError $ NumArgs 1 (length args)

cons :: PrimFunc
cons [v, TList lst] = return $ TList (v : lst)
cons [_, notLst] = throwError $ TypeMismatch ListType (typeOf notLst)
cons args = throwError $ NumArgs 2 (length args)

stringp :: PrimFunc
stringp [String _] = return $ Bool True
stringp args = errorOrFalse 1 args

symbolp :: PrimFunc
symbolp [TSymbol _] = return $ Bool True
symbolp args = errorOrFalse 1 args

numberp :: PrimFunc
numberp [Int _] = return $ Bool True
numberp [Float _] = return $ Bool True
numberp args = errorOrFalse 1 args

listp :: PrimFunc
listp [TList _] = return $ Bool True
listp args = errorOrFalse 1 args

boolp :: PrimFunc
boolp [Bool _] = return $ Bool True
boolp args = errorOrFalse 1 args

typeOfFun :: PrimFunc
typeOfFun [val] = return $ TSymbol (show $ typeOf val)
typeOfFun args = throwError $ NumArgs 1 (length args)

symbolToString :: PrimFunc
symbolToString [TSymbol s] = return $ String s
symbolToString [notSymbol] = throwError $ TypeMismatch SymbolType (typeOf notSymbol)
symbolToString args = throwError $ NumArgs 1 (length args)

numericBinop :: (Int -> Int -> Int) -> [TVal] -> IOThrowsError TVal
numericBinop op params = if length params /= 2 then
                             throwError $ NumArgs 2 (length params)
                         else
                             mapM unpackNum params >>= return . Int . foldl1 op

boolBinop :: (TVal -> IOThrowsError a) -> (a -> a -> Bool) -> [TVal] -> IOThrowsError TVal
boolBinop unpacker op args = if length args /= 2
                                then throwError $ NumArgs 2 (length args)
                                else do left <- unpacker $ head args
                                        right <- unpacker $ args !! 1
                                        return $ Bool $ left `op` right

numBoolBinop :: (Int -> Int -> Bool) -> [TVal] -> IOThrowsError TVal
numBoolBinop = boolBinop unpackNum

unpackNum :: TVal -> IOThrowsError Int
unpackNum (Int n) = return n
unpackNum notNum = throwError $ TypeMismatch IntType (typeOf notNum)

boolBoolBinop :: (Bool -> Bool -> Bool) -> [TVal] -> IOThrowsError TVal
boolBoolBinop = boolBinop unpackBool
  where unpackBool :: TVal -> IOThrowsError Bool
        unpackBool (Bool b) = return b
        unpackBool notBool = throwError $ TypeMismatch BoolType (typeOf notBool)


--strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> IOThrowsError LispVal
--strBoolBinop = boolBinop unpackStr

--unpackStr :: LispVal -> IOThrowsError String
--unpackStr (String s) = return s
--unpackStr notString = throwError $ TypeMismatch "string" notString

eq :: PrimFunc
eq params = if length params /= 2 then
                throwError $ NumArgs 2 (length params)
            else
                eq' (params !! 0) (params !! 1)
  where eq' :: TVal -> TVal -> IOThrowsError TVal
        eq' p1 p2 = return $ Bool (p1 == p2)

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

readForm :: [TVal] -> IOThrowsError TVal
readForm [String form] = readAsSyntax form
readForm [x] = throwError $ TypeMismatch StringType (typeOf x)
readForm args = throwError $ NumArgs 1 (length args)

evalProc :: PrimFexpr
evalProc (Env env) [(Syntax (AnyExpr expr))] cont = do
    eval env expr (EvalCont env cont)
evalProc _ _ _ = throwError $ Default "not implemented"

makeApplication :: [TVal] -> TVal
makeApplication [Syntax fun, Syntax (AnyExpr (List args))] = Syntax $ AnyExpr (Application fun args)

applyProc :: PrimFexpr
applyProc = undefined
--applyProc env [fun,(AnyExpr (List params))] cont = do
--    liftIO $ putStrLn "case1"
--    eval' env fun (ApplyCont params [] env cont)
--applyProc env (fun:params) cont = do
--    liftIO $ putStrLn $ "case2 " ++ show params
--    eval' env fun (ApplyCont params [] env cont)
--applyProc _ params _ = do
--    liftIO $ putStrLn "error case"
--    throwError $ NumArgs 1 (length params)

-- $(deff apply (params)
--    (eval (make-application (first params) (rest params))))

primitiveBindings :: IO Env
primitiveBindings =
    let makeFunc constructor (var, func) = (var, constructor func)
        addPrimitives = flip bindVars $ map (makeFunc PrimFunc) primitives
        addFexprs = flip bindVars $ map (makeFunc PrimFexpr) fexprs
    in nullEnv >>= addPrimitives >>= addFexprs
