{-# LANGUAGE GADTs #-}
module Types where

data Ident
data Lambda
data Application

data Expr a where
    Ident :: String -> Expr Ident
    Lambda :: Expr Ident -> Expr a -> Expr Lambda
    Application :: Expr a -> Expr a -> Expr Application

freeP :: Expr Ident -> Expr a -> Bool
freeP (Ident id) (Ident id') = id == id'
freeP ident@(Ident id) (Lambda (Ident id') expr) = id /= id' && freeP ident expr
freeP id (Application e1 e2) = freeP id e1 || freeP id e2

boundP :: Expr Ident -> Expr a -> Bool
boundP _ (Ident _) = False
boundP i@(Ident id) (Lambda (Ident id') expr) = boundP i expr || (id == id' && freeP i expr)
boundP i (Application e1 e2) = boundP i e1 || boundP i e2

