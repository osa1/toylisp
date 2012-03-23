module Lisp where

import Types
import Parser


quote :: Sexp -> Val
quote (List sexps) = VList $ map quote sexps
quote (Atom symbol) = VSymbol symbol

car :: Params Val -> Val
car (P1 (VList (x:rest))) = x

cdr :: Params Val -> Val
cdr (P1 (VList (x:rest))) = VList rest

cons :: Params Val -> Val
cons (P2 v1 v2) = case v2 of
                    (VList lst) -> VList (v1:lst)
                    otherwise   -> VList [v1, v2]
