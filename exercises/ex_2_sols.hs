--Rose Tree : ノードには任意の子の数のあるツリー
data RoseTree a = RoseLeaf | RoseNode a [RoseTree a]
-- 型を定義し、mapを作って


mapRose :: (a -> b) -> RoseTree a -> RoseTree b
mapRose f RoseLeaf = RoseLeaf
mapRose f (RoseNode a ts) = RoseNode (f a) (map (mapRose f) ts)


data BinaryTree a = Leaf | Node a (BinaryTree a) (BinaryTree a)
    deriving(Show, Functor)

foldrTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldrTree f b Leaf = b
foldrTree f b (Node a l r) = f a (foldrTree f (foldrTree f b r) l)

testTree :: BinaryTree Integer
testTree = Node 0 
    (Node 1 
        (Node 3 Leaf Leaf) 
        Leaf
    ) 
    (Node 2 
        (Node 4 
            (Node 6 Leaf Leaf) 
            (Node 7 Leaf Leaf)
        )
        (Node 5 Leaf Leaf)
    )

levels :: BinaryTree a -> [[a]]
levels Leaf = []
levels (Node a l r) = [a] : zipConcat (levels l) (levels r)
-- levels t = levels' t [[]]
    -- where
        -- levels' :: BinaryTree a -> [[a]] -> [[a]]
        -- levels' Leaf rest = rest
        -- levels' (Node a l r) [] = [a] : levels' l (levels' r [])
        -- levels' (Node a l r) (x:xs) = (a : x) : levels' l (levels' r xs)
zipConcat :: [[a]] -> [[a]] -> [[a]]
zipConcat [] [] = []
zipConcat (x:xs) (y:ys) = (x ++ y) : zipConcat xs ys
zipConcat xs [] = xs
zipConcat [] ys = ys

-- 関数からリストを作る
unfoldList :: (b -> Maybe (a,b)) -> b -> [a]
unfoldList f b = case f b of 
    Nothing -> []
    Just (a,b') -> a : unfoldList f b'

testf :: Integer -> Maybe(Integer, Integer)
-- testf 50 = Nothing
testf x = Just (x,x+1)

-- 関数から木を作りましょう
unfoldTree :: (a -> Maybe (n, a, a)) -> a -> BinaryTree n
unfoldTree f a = case f a of 
    Nothing -> Leaf
    Just (n, x, y) -> Node n (unfoldTree f x) (unfoldTree f y)

-- Check if a tree has the heap property -> Any node is smaller than all of its children
isHeap :: Ord a => BinaryTree a -> Bool
isHeap Leaf = True
isHeap (Node a l r) = case (l, r) of
    (Leaf,Leaf)                     -> True
    (Node al ll rl,Leaf)            -> a <= al && isHeap l
    (Leaf,Node ar lr rr)            -> a <= ar && isHeap r
    (Node al ll rl,Node ar lr rr)   -> a <= al && a <= ar  && isHeap l && isHeap r


fix :: (a -> a) -> a
fix f = f $ fix f
--fixは不動点コンビネータです。これを使って再帰を使わない fib, fact などを作って
fib = fix (\fn x -> if x == 1 || x == 2 then 1 else fn (x-1) + fn (x-2))
fact = fix (\fn x -> if x == 0 then 1 else x * fn (x - 1))

--グラフのデータ型はどうしたらいいでしょうか
-- data Graph a = ???

data Vertex a = Vertex NodeID a
    deriving (Show, Eq, Read)
data Edge = Edge NodeID Weight
    deriving (Show, Eq, Read)
type Weight = Int
type NodeID = Int

newtype Graph a = Graph [(Vertex a, [Edge])]
    deriving (Show)

data Edge' = Edge' NodeID NodeID Weight
data Graph' a = Graph' [Vertex a] [Edge']