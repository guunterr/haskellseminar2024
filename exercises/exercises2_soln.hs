--Rose Tree : ノードには任意の子の数のあるツリー
data RoseTree a = RoseLeaf | RoseNode a [RoseTree a]
-- 型を定義し、mapを作って


mapRose :: (a -> b) -> RoseTree a -> RoseTree b
mapRose f RoseLeaf = RoseLeaf
mapRose f (RoseNode a ts) = RoseNode (f a) (map (mapRose f) ts)

data BinaryTree a = Leaf | Node a (BinaryTree a) (BinaryTree a)
--Binary Treeに対して、以下の関数を作って

-- Right fold on a binary tree
foldrTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldrTree f acc Leaf = acc
foldrTree f acc (Node v l r) = f v (foldrTree f (foldrTree f acc l) r)

-- 木をレベルごとリストにする関数。
-- levels $ Node 1 (Node 2 Leaf (Node 3 Leaf Leaf)) (Node 4 (Node 5 Leaf Leaf) (Node 6 (Node 7 Leaf Leaf) Leaf))
-- = [[1],[2,4],[3,5,6]. [7]]
levels :: BinaryTree a -> [[a]]
levels t = levels' t []
    where
        levels' :: BinaryTree a -> [[a]] -> [[a]]
        levels' Leaf rest = rest
        levels' (Node v l r) [] = [v] : levels' l (levels' r [])
        levels' (Node v l r) (x:xs) = (v:x) : levels' l (levels' r xs)



-- 関数から木を作りましょう
unfoldTree :: (a -> Maybe (n, a, a)) -> a -> BinaryTree n
unfoldTree f a = case f a of
    Nothing -> Leaf
    Just (v,a1,a2) -> Node v (unfoldTree f a1) (unfoldTree f a2)

-- Check if a tree has the heap property -> Any node is smaller than all of its children
isHeap :: Ord a => BinaryTree a -> Bool
isHeap Leaf = True
isHeap (Node v Leaf r@(Node vr _ _)) = v < vr && isHeap r
isHeap (Node v l@(Node vl _ _) Leaf) = v < vl && isHeap l
isHeap (Node v l@(Node vl _ _) r@(Node vr _ _)) = v < vl && v < vr && isHeap l && isHeap r


fix :: (a -> a) -> a
fix f = f $ fix f
--fixは不動点コンビネータです。これを使って再帰を使わない fib, fact などを作って
fib = fix (\fn x -> if x == 1 || x == 2 then 1 else fn (x-1) + fn (x-2))

--グラフのデータ型はどうしたらいいでしょうか
-- newtype Vertex a = Vertex a
--     deriving (Show, Eq)
-- type VertexIndex = Int
-- type Edge = (VertexIndex, VertexIndex)

-- data Graph a = Graph [Vertex a] [Edge]
--     deriving (Show)

newtype Vertex a = Vertex a
    deriving (Show, Eq)
type Edge = Int

newtype Graph a = Graph [(Vertex a, [Edge])]
    deriving (Show)



