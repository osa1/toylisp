{-# OPTIONS_GHC -Wall
                -fno-warn-missing-signatures
                -fno-warn-hi-shadowing
                -fno-warn-unused-do-bind
                -fno-warn-orphans
                -fno-warn-name-shadowing #-}
{-# LANGUAGE GADTs,
             NamedFieldPuns #-}

module TC where

--import Types
--import Env

import qualified Data.Map as M
import Control.Monad.Error
import Control.Monad.State
--import Control.Monad.Identity
import Data.Unique
import Data.List (union, nub)


--import Parser
--import Text.ParserCombinators.Parsec


type TypeError = String
--type TyErr = Either TypeError
type TyErr = ErrorT TypeError IO

data Kind = Star | KFun Kind Kind | Abs
    deriving (Show, Eq)

data Type = TVar TyVar    -- variable
          | TCon TyCon    -- constant
          | TAp Type Type -- application
    deriving (Show, Eq)

type Id = String

instance Show Unique where
    show u = "t" ++ show (hashUnique u)

data TyVar = TyVar Id Kind
    deriving (Show, Eq)

newTyVar :: Kind -> IO TyVar
newTyVar k = liftM (flip TyVar k . show) newUnique

data TyCon = TyCon Id Kind
    deriving (Show, Eq)

tUnit    = TCon (TyCon "()" Star)
tChar    = TCon (TyCon "Char" Star)
tInt     = TCon (TyCon "Int" Star)
tFloat   = TCon (TyCon "Float" Star)

tList    = TCon (TyCon "[]" (KFun Star Star))
tArrow   = TCon (TyCon "(->)" (KFun Star (KFun Star Star)))
tTuple2  = TCon (TyCon "(,)" (KFun Star (KFun Star Star)))

class HasKind t where
    kind :: t -> Kind

instance HasKind TyVar where
    kind (TyVar _ k) = k

instance HasKind TyCon where
    kind (TyCon _ k) = k

instance HasKind Type where
    kind (TVar v) = kind v
    kind (TCon c) = kind c
    kind t@(TAp a1 _) =
        case kind a1 of
            (KFun _ k) -> k
            _ -> error $ show a1 ++ " is not KFun: " ++ show t

type Subst = [(Id, Type)]

nullSubst :: Subst
nullSubst = []

(+->) :: Id -> Type -> Subst
u +-> t = [(u, t)] -- kind u == kind t

class Types t where
    apply :: Subst -> t -> t
    tv    :: t -> [TyVar]

instance Types Type where
    apply s (TVar (TyVar name k)) = case lookup name s of
                           Just t -> t
                           Nothing -> TVar (TyVar name k)
    apply _ (TCon c)  = TCon c
    apply s (TAp a b) = TAp (apply s a) (apply s b)

    tv (TVar u)  = [u]
    tv TCon{}    = []
    tv (TAp a b) = tv a `union` tv b

instance (Types a) => Types [a] where
    apply s = map (apply s)
    tv      = nub . concat . map tv

infixr 4 @@
(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = [ (u, apply s1 t) | (u, t) <- s2 ] ++ s1

mgu :: Type -> Type -> TyErr Subst
mgu (TAp l r) (TAp l' r') = do
    s1 <- mgu l l'
    s2 <- mgu (apply s1 r) (apply s1 r')
    return (s2 @@ s1)
mgu (TVar u) t = varBind u t
mgu t (TVar u) = varBind u t
mgu (TCon tc1) (TCon tc2) | tc1 == tc2 = return nullSubst
mgu t1 t2 = throwError $ "types do not unify: " ++ show t1 ++ ", " ++ show t2

varBind :: TyVar -> Type -> TyErr Subst
varBind u t | t == TVar u      = return nullSubst
            | u `elem` tv t    = throwError "occurs check fails"
            | kind u /= kind t = throwError "kinds do not match"
            | otherwise        = let (TyVar name _) = u in return (name +-> t)

data Expr = Var Id
          | Lit Literal
          | Ap Expr Expr 
          -- | Let Bindings Expr
    deriving Show

data Literal = LitInt Integer
             | LitChar Char
             | LitStr String
    deriving Show

type TyEnv = [(String, Type)]
type NonGenerics = [String]

unify :: Subst -> Type -> Type -> TyErr Type
unify subst t1 t2 = do
    subst' <- mgu (apply subst t1) (apply subst t2)
    return $ apply subst' t1

freshType :: NonGenerics -> Type -> IO Type
freshType nongen t@(TVar (TyVar name k)) =
    if name `elem` nongen
        then return t
        else do new <- newTyVar k
                return $ TVar new
freshType nongen (TAp t1 t2) = do
    t1' <- freshType nongen t1
    t2' <- freshType nongen t2
    return (TAp t1' t2')
freshType _ t = return t

retrieve :: TyEnv -> Subst -> NonGenerics -> Id -> TyErr Type
retrieve env subst nongen var =
    case lookup var subst of
        Nothing -> case lookup var env of
                       Nothing -> throwError $ "unbound var: " ++ show var
                       Just t  -> liftIO $ freshType nongen t
        Just t -> return t

analyzeExp :: TyEnv -> Subst -> NonGenerics -> Expr -> TyErr Type
analyzeExp env subst nongen (Var name) = retrieve env subst nongen name
analyzeExp _ _ _ (Lit LitInt{})  = return tInt
analyzeExp _ _ _ (Lit LitChar{}) = return tChar
analyzeExp _ _ _ (Lit LitStr{})  = return (TAp tList tChar)
analyzeExp env subst nongen (Ap e1 e2) = do
    funty <- analyzeExp env subst nongen e1
    argty <- analyzeExp env subst nongen e2
    res <- liftIO $ newTyVar Abs
    unify subst funty (TAp (TAp tArrow argty) (TVar res))

--main :: IO ()
--main = do
    --let exp1 = Ap (Var "a") (Var "a")
    --ty <- runErrorT $ analyzeExp [] [] [] exp1
    --putStrLn $ show ty
