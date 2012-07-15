-- Eq instance for Expr GADT
{-# LANGUAGE GADTs #-}

module Eq where

import Types

instance Eq TVal where
    -- Maybe I should throw an error for testing continuations and functions
    Char c1 == Char c2 = c1 == c2
    String s1 == String s2 = s1 == s2
    TSymbol s1 == TSymbol s2 = s1 == s2
    Int i1 == Int i2 = i1 == i2
    Float f1 == Float f2 = f1 == f2
    Bool b1 == Bool b2 = b1 == b2
    Nil == Nil = True
    t1 == t2 = if typeOf t1 /= typeOf t2 then False else undefined


instance Eq (Expr a) where
    Symbol s1 == Symbol s2 = s1 == s2
    Lambda p1 b1 == Lambda p2 b2 = p1 == p2 && b1 == b2
    Application f1 b1 == Application f2 b2 = f1 == f2 && b1 == b2
    If i1 t1 e1 == If i2 t2 e2 = i1 == i2 && t1 == t2 && e1 == e2
    Fexpr n1 p1 == Fexpr n2 p2 = n1 == n2 && p1 == p2
    Val v1 == Val v2 = v1 == v2
    List e1 == List e2 = e1 == e2
    CallCC c1 == CallCC c2 = c1 == c2

    _ == _ = False

instance Eq AnyExpr where
    AnyExpr s1@Symbol{} == AnyExpr s2@Symbol{} = s1 == s2
    AnyExpr l1@Lambda{} == AnyExpr l2@Lambda{} = l1 == l2
    AnyExpr a1@Application{} == AnyExpr a2@Application{} = a1 == a2
    AnyExpr if1@If{} == AnyExpr if2@If{} = if1 == if2
    AnyExpr f1@Fexpr{} == AnyExpr f2@Fexpr{} = f1 == f2
    AnyExpr v1@Val{} == AnyExpr v2@Val{} = v1 == v2
    AnyExpr c1@CallCC{} == AnyExpr c2@CallCC{} = c1 == c2

    _ == _ = False

