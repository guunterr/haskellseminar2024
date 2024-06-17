import Data.Char (isNumber, isAlphaNum, isAlpha, isSpace)
import Data.Bifunctor (Bifunctor(bimap))
--数式のParserをつくって
type ID = String
type Const = Integer
data Op = Add | Mul | Sub
    deriving (Show, Eq)
data Expr = Const Const | Var ID | Apply Op Expr Expr
    deriving (Show, Eq)

--Dijkstra's Shunting Yard Algorithm

parse :: String -> Either String Expr
parse s = tokenize s >>= ast
--モナッドつかえた！！！
-- parse "((3 + 5) - 2)" = Apply (Sub) (Apply (Add) (Const 3) (Const 5)) (Const 2))

data Token = Number Const | Variable String | LBracket | RBracket | Plus | Minus | Times | Slash
    deriving (Show, Eq)
tokenize :: String -> Either String [Token]
-- tokenize "((3 + 5) - 2)" = [LBracket, LBracket, 3, +, 5, RBracket, ...]
tokenize [] = Right []
tokenize ('(':s) = (LBracket :) <$> tokenize s
tokenize (')':s) = (RBracket :) <$> tokenize s
tokenize ('+':s) = (Plus :) <$> tokenize s
tokenize ('-':s) = (Minus :) <$> tokenize s
tokenize ('*':s) = (Times :) <$> tokenize s
tokenize ('/':s) = (Slash :) <$> tokenize s
tokenize s@(c:s')
    | isNumber c = uncurry (fmap . (:)) . bimap (Number . read) tokenize $ lexNum "" s
    | isAlpha c = uncurry (fmap . (:)) . bimap Variable tokenize $ lexVar "" s
    | isSpace c = tokenize s'
tokenize (c:s) = Left $ "Token Error: Unexpected Character \'" ++ [c] ++ "\' With remaining string \"" ++ s ++ "\""

lexNum :: String -> String -> (String, String)
lexNum n "" = (reverse n , "")
lexNum n s@(c:s') 
    | isNumber c = lexNum (c:n) s'
    | otherwise  = (reverse n, s)

lexVar :: String -> String -> (String, String)
lexVar n "" = (reverse n, "")
lexVar n s@(c:s') 
    | isAlphaNum c = lexVar (c:n) s'
    | otherwise  = (reverse n, s)

ast :: [Token] -> Either String Expr
ast = undefined

rpn :: Expr -> String
-- Reverse polish notation
rpn = undefined

show :: Expr -> String
--きれいに数式を表示して
show = undefined

--Parsingに興味ない人はここからはじめてもいいよ

--代入
substitute :: [(ID, Const)] -> Expr -> Expr
substitute = undefined

eval :: Expr -> Maybe Const
eval = undefined

diff :: ID -> Expr -> Expr
diff = undefined
--diff "x" 3x^2 + 2yx + 5 = 6x + 2y
-- 偏微分演算子
int :: ID -> Expr -> Expr
int = undefined

-- solve 型は自由、とりあえずなんか頑張ってね
-- Factorize (方程式をx=の形にする)など