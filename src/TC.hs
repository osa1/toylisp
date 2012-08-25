{-# OPTIONS_GHC -Wall
                -fno-warn-missing-signatures
                -fno-warn-hi-shadowing
                -fno-warn-unused-do-bind
                -fno-warn-orphans
                -fno-warn-name-shadowing #-}
{-# LANGUAGE GADTs,
             NamedFieldPuns,
             TypeSynonymInstances
             #-}

module TC where

--import Types
--import Env

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Error
import Control.Monad.State
--import Control.Monad.Identity
import Data.Unique


type TypeError = String
--type TyErr = Either TypeError
type TyErr = ErrorT TypeError IO

data Type = TVar TyVar     -- variable
          | TCon TyCon     -- constant
          | TArr Type Type -- arrow
    deriving (Show, Eq)

type Id = String

data TyCon = TyCon Id
    deriving (Show, Eq)

instance Show Unique where
    show u = "t" ++ show (hashUnique u)

data TyVar = TyVar Id
    deriving (Show, Eq, Ord)

newTyVar :: IO TyVar
newTyVar = liftM (TyVar . show) newUnique

tUnit  = TCon (TyCon "Unit")
tChar  = TCon (TyCon "Char")
tInt   = TCon (TyCon "Int")
tFloat = TCon (TyCon "Float")
tBool  = TCon (TyCon "Bool")

type Subst = M.Map Id Type

nullSubst :: Subst
nullSubst = M.empty

(+->) :: Id -> Type -> Subst
u +-> t = M.singleton u t

class Types t where
    apply :: Subst -> t -> t
    tv    :: t -> S.Set TyVar

instance Types Type where
    apply s (TVar (TyVar name)) = case M.lookup name s of
                           Just t -> t
                           Nothing -> TVar (TyVar name)
    apply _ (TCon c)  = TCon c
    apply s (TArr a b) = TArr (apply s a) (apply s b)

    tv (TVar u)  = S.singleton u
    tv TCon{}    = S.empty
    tv (TArr a b) = tv a `S.union` tv b

instance (Types a) => Types [a] where
    apply s = map (apply s)
    tv      = S.unions . map tv

infixr 4 @@
(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = M.map (apply s1) s2 `M.union` s1

mgu :: Type -> Type -> TyErr Subst
mgu (TArr l r) (TArr l' r') = do
    s1 <- mgu l l'
    s2 <- mgu (apply s1 r) (apply s1 r')
    return (s2 @@ s1)
mgu (TVar u) t = varBind u t
mgu t (TVar u) = varBind u t
mgu (TCon tc1) (TCon tc2) | tc1 == tc2 = return nullSubst
mgu t1 t2 = throwError $ "types do not unify: " ++ show t1 ++ ", " ++ show t2

varBind :: TyVar -> Type -> TyErr Subst
varBind u t | t == TVar u       = return nullSubst
            | u `S.member` tv t = throwError "occurs check fails"
            -- | kind u /= kind t = throwError "kinds do not match"
            | otherwise         = let (TyVar name) = u in return (name +-> t)


--data Type = TVar TyVar     -- variable
--          | TCon TyCon     -- constant
--          | TArr Type Type -- arrow
--    deriving (Show, Eq)

--type Subst = M.Map Id Type

main :: IO ()
main = do
    --let env = TyEnv (M.fromList [("test", TArr tFloat tFloat), ("test2", TArr (TVar (TyVar "a")) (TVar (TyVar "a")))])
    --putStrLn $ show (tv env)
    --let env' = apply (M.fromList [("a", tInt)]) env
    --putStrLn $ show (tv env')
    --ty <- freshType S.empty (TArr (TVar (TyVar "a")) (TVar (TyVar "a")))
    --putStrLn $ show ty
    let ty1 = TArr tInt tBool
    let ty2 = TArr (TVar (TyVar "a")) (TVar (TyVar "b"))
    r <- runErrorT $ mgu ty1 ty2
    putStrLn $ show r

data Expr = Var Id
          | Lit Literal
          | Ap Expr Expr
          | Let Bindings Expr
    deriving Show

data Bindings = LetBinding String Expr
              | LambdaBinding String
    deriving Show

data Literal = LitInt Integer
             | LitChar Char
             | LitBool Bool
    deriving Show

--type TyEnv = M.Map String Type
newtype TyEnv = TyEnv (M.Map String Type)
type NonGenerics = S.Set Id
type CopyEnv = M.Map String Type

instance Types TyEnv where
    apply s (TyEnv t) = TyEnv (M.map (apply s) t)
    tv (TyEnv t) = S.unions (map tv $ M.elems t)

freshType :: NonGenerics -> Type -> IO Type
freshType nongen ty = liftM snd (freshType' nongen M.empty ty)
  where freshType' :: NonGenerics -> CopyEnv -> Type -> IO (CopyEnv, Type)
        freshType' nongen cenv t@(TVar (TyVar name))
            | name `S.member` nongen = return (cenv, t)
            | otherwise = case M.lookup name cenv of
                            Nothing -> do
                                new <- newTyVar
                                return $ (M.insert name (TVar new) cenv, TVar new)
                            Just t -> return (cenv, t)
        freshType' nongen cenv (TArr t1 t2) = do
            (cenv', t1') <- freshType' nongen cenv t1
            (cenv'', t2') <- freshType' nongen cenv' t2
            return (cenv'', TArr t1' t2')
        freshType' _ cenv t = return (cenv, t)

--ti :: TyEnv -> NonGenerics -> Expr -> TyErr (Subst, Type)
--ti env nongen (Var name) =
--    case M.lookup name env of
--        Nothing -> throwError $ "unbound var: " ++ name
--        Just t  -> do
--            fresh <- liftIO $ freshType nongen t
--            return (nullSubst, t)
--ti _ _ (Lit LitInt{})  = return (nullSubst, tInt)
--ti _ _ (Lit LitChar{}) = return (nullSubst, tChar)
--ti _ _ (Lit LitBool{}) = return (nullSubst, tBool)
--ti env nongen (Ap e1 e2) = do
--    (s1, e1ty) <- ti env nongen e1
--    (s2, e2ty) <- ti (apply s1 env) nongen e2
--    var <- liftIO newTyVar
--    s <- mgu (apply s2 e1ty) (TArr e2ty (TVar var))
--    return (s @@ s2 @@ s1, apply s (TVar var))

--ti env nongen (Let (LetBinding name e1) e2) = do
--    (s1, t)  <- ti env nongen e1
--    (s2, t2) <- ti (apply s1 (M.insert name t env)) nongen e2
--    return (s2 @@ s1, t2)

----ti env nongen (Let (LambdaBinding name) e) = do
--    --var <- liftIO newTyVar
--    --(s, t) <- ti env (S.insert name nongen) e
--    --return (s, TArr (apply s var)


