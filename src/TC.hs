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

type Id = String
type TRecord = M.Map Id Type

data Type = TVar TyVar     -- variable
          | TCon TyCon     -- constant
          | TArr Type Type -- arrow
          | TRec TRecord   -- record
    deriving (Show, Eq)


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
    apply _ (TCon c)   = TCon c
    apply s (TArr a b) = TArr (apply s a) (apply s b)
    apply s (TRec ts)  = TRec (M.map (apply s) ts)

    tv (TVar u)  = S.singleton u
    tv TCon{}    = S.empty
    tv (TArr a b) = tv a `S.union` tv b
    tv (TRec ts) = S.unions (map tv (M.elems ts))

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
mgu (TRec r1) (TRec r2) = do
    let keys = M.keysSet r1 `S.intersection` M.keysSet r2
    unifyVals (S.toList keys) nullSubst
  where unifyVals :: [Id] -> Subst -> TyErr Subst
        unifyVals [] substs = return substs
        unifyVals (x:xs) subst = do
            let (Just t1) = M.lookup x r1
                (Just t2) = M.lookup x r2
            subst' <- mgu (apply subst t1) (apply subst t2)
            unifyVals xs (subst' @@ subst)
mgu (TVar u) t = varBind u t
mgu t (TVar u) = varBind u t
mgu (TCon tc1) (TCon tc2) | tc1 == tc2 = return nullSubst
mgu t1 t2 = throwError $ "types do not unify: " ++ show t1 ++ ", " ++ show t2

varBind :: TyVar -> Type -> TyErr Subst
varBind u t | t == TVar u       = return nullSubst
            | u `S.member` tv t = throwError "occurs check fails"
            -- | kind u /= kind t = throwError "kinds do not match"
            | otherwise         = let (TyVar name) = u in return (name +-> t)


data Expr = Var Id
          | Lit Literal
          | Ap Expr Expr
          | Let Bindings Expr
          | If Expr Expr Expr
          | Rec [(Id, Expr)]
          | Select Expr Id
    deriving Show

data Bindings = LetBinding String Expr
              | LambdaBinding String
    deriving Show

data Literal = LitInt Integer
             | LitChar Char
             | LitBool Bool
    deriving Show

newtype TyEnv = TyEnv (M.Map String Type) deriving Show
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

retrieve :: TyEnv -> NonGenerics -> String -> TyErr Type
retrieve (TyEnv env) nongen name =
      case M.lookup name env of
          Just t -> liftIO $ freshType nongen t
          Nothing -> throwError $ "unbound var: " ++ name

          -- | Rec [(Name, Expr)]
          -- | Select Expr Id

ti :: TyEnv -> NonGenerics -> Expr -> TyErr (Subst, Type)
ti env nongen (Var name) = liftM ((,) nullSubst) (retrieve env nongen name)
ti _ _ (Lit LitInt{})  = return (nullSubst, tInt)
ti _ _ (Lit LitChar{}) = return (nullSubst, tChar)
ti _ _ (Lit LitBool{}) = return (nullSubst, tBool)

ti env nongen (Ap e1 e2) = do
    (s1, e1ty) <- ti env nongen e1
    (s2, e2ty) <- ti env nongen e2
    var <- liftIO newTyVar
    s <- mgu (TArr e2ty (TVar var)) e1ty
    return (s @@ s2 @@ s1, apply s (TVar var))
ti (TyEnv env) nongen (Let (LetBinding name e1) e2) = do
    var <- liftIO newTyVar
    let tyenv' = TyEnv (M.insert name (TVar var) env)
    (s1, t)  <- ti tyenv' nongen e1
    s1' <- mgu (TVar var) (apply s1 t)
    (s2, t2) <- ti (apply s1' tyenv') nongen e2
    return (s2 @@ s1', apply s2 t2)
ti (TyEnv env) nongen (Let (LambdaBinding name) e) = do
    var@(TyVar n) <- liftIO newTyVar
    let tyenv' = TyEnv (M.insert name (TVar var) env)
    liftIO $ putStrLn $ show tyenv'
    (s1, t)  <- ti tyenv' (S.insert n nongen) e
    return (s1, apply s1 (TArr (TVar var) t))
ti env nongen (If e1 e2 e3) = do
    (s1, t1) <- ti env nongen e1
    s1' <- mgu (apply s1 t1) tBool
    (s2, t2) <- ti (apply s1' env) nongen e2
    (s3, t3) <- ti (apply s1' env) nongen e3
    s <- mgu (apply s2 t2) (apply s3 t3)
    return (s, apply s t2)

ti env nongen (Rec elems) = do
    (s, ts) <- collectTypes env nongen elems [] nullSubst
    return (s, TRec $ M.fromList ts)
  where collectTypes :: TyEnv -> NonGenerics -> [(Id, Expr)] -> [(String, Type)] -> Subst -> TyErr (Subst, [(String, Type)])
        collectTypes env nongen ((name,expr):xs) r subst = do
            (s, t) <- ti (apply subst env) nongen expr
            collectTypes env nongen xs ((name,t):r) (s @@ subst)
        collectTypes _ _ [] r s = return (s, r)

ti env nongen (Select e id) = do
    (s, t) <- ti env nongen e
    var <- liftIO newTyVar
    let r' = TRec (M.singleton id (TVar var))
    s' <- mgu r' (apply s t)
    return (s' @@ s, apply s' $ TVar var)


main :: IO ()
main = do
    let env = TyEnv (M.fromList [ ("+", TArr tInt (TArr tInt tInt))
                                , ("id", TArr (TVar (TyVar "a")) (TVar (TyVar "a")))
                                ] )

    --let exp = If (Ap (Var "id") (Lit (LitBool True)))
    --             (Ap (Ap (Var "+") (Ap (Var "id") (Lit (LitInt 10)))) (Ap (Var "id") (Lit (LitInt 10))))
    --             (Lit (LitInt 123))
    let exp = Let (LambdaBinding "a")
                  (Rec [ ("a", (Lit (LitInt 10)))
                       , ("b", (Var "+"))
                       , ("c", (Ap (Ap (Var "+") (Var "a")) (Lit (LitInt 20))))
                       , ("d", (Var "a"))
                       ])
    let exp2 = Ap exp (Lit (LitInt 20))
    --let exp3 = Select exp2 "d"
    let exp3 = Ap (Ap (Var "+") (Select exp2 "b")) (Lit (LitInt 20))


    ty <- runErrorT $ ti env S.empty exp3
    putStrLn $ show (liftM fst ty)
    putStrLn $ show (liftM snd ty)

