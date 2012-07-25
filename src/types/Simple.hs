{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-hi-shadowing -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}
{-# LANGUAGE GADTs #-}

module Simple where

import Control.Monad.Error
import Control.Monad.Identity

data Term = TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmVar Int Int
          | TmAbs String Ty Term
          | TmApp Term Term



data Ty = TyBool
        | TyInt
        | TyArr Ty Ty
    deriving (Show, Eq)



type Context = [(String, Binding)]
data Binding = VarBind Ty

addBinding :: Context -> String -> Binding -> Context
addBinding ctx n b = (n, b):ctx

getType :: Context -> String -> Maybe Ty
getType ctx n = fmap (\(VarBind ty) -> ty) $ lookup n ctx

getTypePos :: Context -> Int -> Maybe Ty
getTypePos ctx 1 = if length ctx == 0 then
                       Nothing
                    else
                        Just $ (\(VarBind ty) -> ty) (snd (head ctx))
getTypePos ctx n = getTypePos (tail ctx) (n-1)

data TyErr = TyErr Ty Ty
           | TyMsg String
    deriving Show

instance Error TyErr where
    noMsg = TyMsg "An error has occured"
    strMsg = TyMsg

type TypeError = Either TyErr
--type TypeError = ErrorT TyErr Identity

typeOf :: Context -> Term -> TypeError Ty
typeOf _ (TmTrue) = return TyBool
typeOf _ (TmFalse) = return TyBool
typeOf ctx (TmIf guard t e) = do
    gt <- typeOf ctx guard
    if gt == TyBool then
        do tt <- typeOf ctx t
           te <- typeOf ctx e
           if tt == te then return tt else throwError $ TyErr tt te
    else
        throwError $ TyMsg "guard is not boolean"
typeOf ctx (TmVar s _) =
    case getTypePos ctx s of
        Nothing -> throwError $ TyMsg "var is not in context"
        Just t -> return t
typeOf ctx (TmAbs name ty term) =
    let ctx' = addBinding ctx name (VarBind ty)
    in do termty <- typeOf ctx' term
          return $ TyArr ty termty
typeOf ctx (TmApp t1 t2) = do
    tyt1 <- typeOf ctx t1
    tyt2 <- typeOf ctx t2
    case tyt1 of
        (TyArr paramty returnty) -> if tyt2 == paramty then
                                        return returnty
                                    else
                                        throwError $ TyErr paramty tyt2
        _ -> throwError $ TyMsg "arrow type expected"

tests :: [TypeError Ty]
tests = [ typeOf [("bir", (VarBind TyBool)), ("iki", (VarBind TyBool))] (TmIf TmTrue (TmVar 1 1) (TmVar 2 1))
        , typeOf [("bir", (VarBind TyBool)), ("iki", (VarBind TyInt))] (TmIf TmTrue (TmVar 1 1) (TmVar 2 1))
        , typeOf [("p", (VarBind TyInt))] (TmApp (TmAbs "p" TyInt TmTrue) (TmVar 1 1))
        , typeOf [("p", (VarBind TyBool))] (TmApp (TmAbs "p" TyInt TmTrue) (TmVar 1 1))
        ]

main :: IO ()
main = do
    mapM (putStrLn . show) tests
    return ()






