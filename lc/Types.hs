{-# LANGUAGE GADTs #-}
module Types where

import Data.Set as S

data Ident
data Lambda
data Application

type Var = String

data Expr a where
    Ident :: String -> Expr Ident
    Lambda :: Expr Ident -> AnyExpr -> Expr Lambda
    Application :: AnyExpr -> AnyExpr -> Expr Application

data AnyExpr where
    AnyExpr :: Expr a -> AnyExpr

freeP :: Expr Ident -> Expr a -> Bool
freeP (Ident id) (Ident id') = id == id'
freeP ident@(Ident id) (Lambda (Ident id') (AnyExpr expr)) = id /= id' && freeP ident expr
freeP id (Application (AnyExpr e1) (AnyExpr e2)) = freeP id e1 || freeP id e2

boundP :: Expr Ident -> Expr a -> Bool
boundP _ (Ident _) = False
boundP i@(Ident id) (Lambda (Ident id') (AnyExpr expr)) = boundP i expr || (id == id' && freeP i expr)
boundP i (Application (AnyExpr e1) (AnyExpr e2)) = boundP i e1 || boundP i e2
