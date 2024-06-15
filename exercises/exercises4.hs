--数式のParserをつくって
type ID = String
type Const = Double
data Op = Add | Mul | Sub | Div
data Expr = Const Const | Var ID | Apply Op Expr Expr

--Dijkstra's Shunting Yard Algorithm

parse :: String -> Maybe Expr
parse s = tokenize s >>= ast
--モナッドつかえた！！！
-- parse "((3 + 5) - 2)" = Apply (Sub) (Apply (Add) (Const 3) (Const 5)) (Const 2))

data Token = Number Const | Variable String | LBracket | RBracket | Plus | Minus | Times 
tokenize :: String -> Maybe [Token]
-- tokenize "((3 + 5) - 2)" = [LBracket, LBracket, 3, +, 5, RBracket, ...]
tokenize = undefined

ast :: [Token] -> Maybe Expr
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