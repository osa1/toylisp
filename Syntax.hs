{-# OPTIONS_GHC -Wall -fno-warn-hi-shadowing -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}
{-# LANGUAGE GADTs #-}
module Syntax where

import Types
import Control.Monad.Error (throwError)


type SyntaxFun = [AnyExpr] -> IOThrowsError AnyExpr

makeApplication :: SyntaxFun
makeApplication (first:rest) = return $ AnyExpr (Application first rest)
makeApplication _ = throwError $ SyntaxError




