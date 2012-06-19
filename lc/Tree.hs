{-# LANGUAGE GADTs #-}
module Tree where


data (Ord e) => Node e = Node e
    deriving (Eq, Ord, Show)

data BinTree e = BinTree (Node e) (BinTree e) (BinTree e)
               | Empty
    deriving (Show)

empty :: (Eq e, Ord e) => BinTree e
empty = Empty

insert :: Ord e => BinTree e -> e -> BinTree e
insert Empty e = BinTree (Node e) Empty Empty
insert tree@(BinTree node@(Node e) l r) n
    | n == e = tree
    | n < e  = BinTree node (insert l n) r
    | otherwise = BinTree node l (insert r n)

{-delete :: Ord e => BinTree e -> e -> BinTree e-}
{-delete Empty _ = Empty-}
{-delete (BinTree node@(Node s) l r) e = undefined-}

{-data (Show a) => Expr a = I Int-}
            {-| B Bool-}
            {-| Add (Expr a) (Expr a)-}
            {-| Mul (Expr a) (Expr a)-}
            {-| Eq  (Expr a) (Expr a)-}
    {-deriving (Show)-}

{-add :: Expr Int -> Expr Int -> Expr Int-}
{-add = Add-}

{-i :: Int -> Expr Int-}
{-i = I-}

{-b :: Bool -> Expr Bool-}
{-b = B-}

{-eval :: Expr a -> a-}
{-eval (I n) = n-}
{-eval (B n) = n-}


data B
data I

data Expr a = Bl Bool | In Int
bl :: Bool -> Expr B
bl bool = Bl bool

eval (Bl b) = B
eval (In i) = I


{-data Expr a where-}
    {-I :: Int -> Expr Int-}
    {-B :: Bool -> Expr Bool-}
    {-Add :: Expr Int -> Expr Int -> Expr Int-}
    {-Mul :: Expr Int -> Expr Int -> Expr Int-}
    {-Eq :: Expr Int -> Expr Int -> Expr Bool-}

{-eval :: Expr a -> a-}
{-eval (I n) = n-}
{-eval (B b) = b-}
{-eval (Add e1 e2) = eval e1 + eval e2-}
{-eval (Mul e1 e2) = eval e1 * eval e2-}
{-eval (Eq  e1 e2) = eval e1 == eval e2-}


