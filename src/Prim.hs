{-# OPTIONS_GHC -Wall -fno-warn-hi-shadowing -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}
{-# LANGUAGE GADTs #-}
module Prim where

import Types
--import Control.Monad.Error (throwError)
--import IO
--import Parser (readAsSyntax)
import Env
--import Eval (eval)
--import Eq ()
--import Syntax

primitives :: [(String, TFunc)]
primitives = [
             --  ("first", first)
             --, ("rest", rest)
             --, ("cons", cons)
             --, ("eq", eq)

               ("+", PrimFunc (numericBinop (+)) boolint)
             , ("-", PrimFunc (numericBinop (-)) boolint)
             , ("*", PrimFunc (numericBinop (*)) boolint)
             , ("div", PrimFunc (numericBinop div) boolint)
             , ("mod", PrimFunc (numericBinop mod) boolint)
             , ("quot", PrimFunc (numericBinop quot) boolint)
             , ("rem", PrimFunc (numericBinop rem) boolint)

             , ("=", PrimFunc (numBoolBinop (==)) boolintbool)
             , ("<", PrimFunc (numBoolBinop (<)) boolintbool)
             , (">", PrimFunc (numBoolBinop (>)) boolintbool)
             , ("/=", PrimFunc (numBoolBinop (/=)) boolintbool)
             , (">=", PrimFunc (numBoolBinop (>=)) boolintbool)
             , ("<=", PrimFunc (numBoolBinop (<=)) boolintbool)
             --, ("&&", PrimFunc (boolBoolBinop (&&)) boolint)
             --, ("||", PrimFunc (boolBoolBinop (||)) boolint)

             --, ("string=?", strBoolBinop (==))
             --, ("string<?", strBoolBinop (<))
             --, ("string>?", strBoolBinop (>))
             --, ("string<=?", strBoolBinop (<=))
             --, ("string>=?", strBoolBinop (>=))

             --, ("string?", stringp)
             --, ("symbol?", symbolp)
             --, ("number?", numberp)
             --, ("list?", listp)
             --, ("boolean?", boolp)
             --, ("symbol->string", symbolToString)
             -- , ("apply", applyProc)

             --, ("type-of", typeOfFun)

             ---- IO functions
             --, ("println", println)
             --, ("open-input-file", makePort ReadMode)
             --, ("open-output-file", makePort WriteMode)
             --, ("close-input-port", closePort)
             --, ("close-output-port", closePort)
             --, ("read", readProc)
             --, ("write", writeProc)
             --, ("read-contents", readContents)
             --, ("read-all", readAll)

             -- read
             --, ("read", readForm)

             --, ("make-application", makeApplicationSyntax)
             -- eval
             --("eval", evalForm)
             ]
  where boolint = FuncTy [("a", IntTy), ("b", IntTy)] IntTy
        boolintbool = FuncTy [("a", IntTy), ("b", IntTy)] BoolTy

--first :: PrimFunc
--first [TList lst] = return $ head lst
--first [notList] = throwError $ TypeMismatch LstTy (typeOf notList)
--first args = throwError $ NumArgs 1 (length args)

--rest :: PrimFunc
--rest [TList lst] = return $ TList (tail lst)
--rest [notList] = throwError $ TypeMismatch LstTy (typeOf notList)
--rest args = throwError $ NumArgs 1 (length args)

--cons :: PrimFunc
--cons [v, TList lst] = return $ TList (v : lst)
--cons [_, notLst] = throwError $ TypeMismatch LstTy (typeOf notLst)
--cons args = throwError $ NumArgs 2 (length args)

--stringp :: PrimFunc
--stringp [String _] = return $ Bool True
--stringp args = errorOrFalse 1 args

--symbolp :: PrimFunc
--symbolp [TSymbol _] = return $ Bool True
--symbolp args = errorOrFalse 1 args

--numberp :: PrimFunc
--numberp [Int _] = return $ Bool True
--numberp [Float _] = return $ Bool True
--numberp args = errorOrFalse 1 args

--listp :: PrimFunc
--listp [TList _] = return $ Bool True
--listp args = errorOrFalse 1 args

--boolp :: PrimFunc
--boolp [Bool _] = return $ Bool True
--boolp args = errorOrFalse 1 args

--typeOfFun :: PrimFunc
--typeOfFun [val] = return $ TSymbol (show $ typeOf val)
--typeOfFun args = throwError $ NumArgs 1 (length args)

--symbolToString :: PrimFunc
--symbolToString [TSymbol s] = return $ String s
--symbolToString [notSymbol] = throwError $ TypeMismatch SymbolTy (typeOf notSymbol)
--symbolToString args = throwError $ NumArgs 1 (length args)

unpackInt :: TVal -> Int
unpackInt = \(Int i) -> i

numericBinop :: (Int -> Int -> Int) -> [TVal] -> IOThrowsError TVal
numericBinop op params = return (Int $ op (unpackInt (params !! 0)) (unpackInt (params !! 1)))

numBoolBinop :: (Int -> Int -> Bool) -> [TVal] -> IOThrowsError TVal
numBoolBinop op params = return (Bool $ op (unpackInt (params !! 0)) (unpackInt (params !! 1)))

primitiveBindings :: Env TVal
primitiveBindings = newEnv $ map (\(n,f) -> (n, TFunc f)) primitives
