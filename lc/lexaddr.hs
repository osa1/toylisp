{-# LANGUAGE GADTs, StandaloneDeriving #-}
import Text.ParserCombinators.Parsec hiding (State(..))
{-import Control.Monad.State-}
import Control.Applicative ((<*))
import Data.Set hiding (map)
import Data.List (intercalate)

data Ident
data Lambda
data Application
data If

type Var = String

data Expr a where
    Ident :: String -> Expr Ident
    If :: AnyExpr -> AnyExpr -> AnyExpr -> Expr If
    Lambda :: [Expr Ident] -> AnyExpr -> Expr Lambda
    Application :: [AnyExpr] -> Expr Application

instance Show (Expr a) where
    show (Ident id) = id
    show (If e1 e2 e3) = "(if " ++ (intercalate " " [show e1, show e2, show e3]) ++ ")"
    show (Lambda params expr) = "(lambda (" ++ (intercalate " " $ map show params) ++ ") " ++ show expr ++ ")"
    show (Application exprs) = "(" ++ (intercalate " " $ map show exprs) ++ ")"

data AnyExpr where
    AnyExpr :: Expr a -> AnyExpr

instance Show AnyExpr where
    show (AnyExpr e) = show e

{-data CompilerState = CompilerState { indent :: Int }-}
data CompilerState = CompilerState { indent :: Int }

class Compilable a where
    {-compile :: a -> State CompilerState String-}
    compile :: CompilerState -> a -> String

indentedLine :: CompilerState -> String -> String
indentedLine (CompilerState indent) str = replicate (indent*2) ' ' ++ str

instance Compilable (Expr a) where
    compile cs (Ident id) = indentedLine cs id

    compile cs@(CompilerState indent) (If ifE thenE elseE) =
        indentedLine cs $ "if " ++ compile (CompilerState 0) ifE ++ " then\n" ++
            compile (CompilerState $ indent+2) thenE ++ "\n" ++
            indentedLine cs "else\n" ++ compile (CompilerState $ indent+2) elseE

    compile cs@(CompilerState indent) (Lambda args expr) =
        indentedLine cs $ "fun " ++ (intercalate " " $ map (compile cs) args) ++ " ->\n" ++
            compile (CompilerState $ indent+2) expr

    compile cs@(CompilerState indent) (Application exprs) =
        indentedLine cs $ intercalate " " $ map (compile cs) exprs
        

instance Compilable AnyExpr where
    compile cs (AnyExpr e) = compile cs e

--------------------------------------------------------------------------------

wrapSpaces parser = spaces >> (parser <* spaces)

ident :: Parser (Expr Ident)
ident = do
    first <- oneOf $ ['a'..'z'] ++ ['A'..'Z']
    rest <- many $ oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ "-_/\\*+?"
    return $ Ident $ [first] ++ rest

lambda :: Parser (Expr Lambda)
lambda = do
    char '('
    wrapSpaces $ string "_lambda"
    char '('
    params <- ident `sepBy` spaces
    char ')'
    ex <- expr
    char ')'
    return $ Lambda params ex

application :: Parser (Expr Application)
application = do
    char '('
    exprs <- expr `sepBy` spaces
    char ')'
    return $ Application exprs

ifExpr :: Parser (Expr If)
ifExpr = do
    char '('
    wrapSpaces $ string "_if"
    cond <- expr
    thenCase <- expr
    elseCase <- expr
    char ')'
    return $ If cond thenCase elseCase

anyExpr :: Parser (Expr a) -> Parser AnyExpr
anyExpr p = fmap AnyExpr p

expr :: Parser AnyExpr
expr = wrapSpaces $ choice [anyExpr $ try ident, anyExpr $ try lambda, anyExpr $ try ifExpr, anyExpr $ try application]

--------------------------------------------------------------------------------

test = let st = parse expr "test" "(_if a b c)"
       in case st of
             Right expr -> putStrLn $ compile (CompilerState 0) expr
             {-Right expr -> putStrLn $ show expr-}
             Left err -> putStrLn $ show err

