--Rose Tree : ノードには任意の子の数のあるツリー
-- data RoseTree a = ...
-- 型を定義し、mapを作って

data BinaryTree a = Leaf | Node a (BinaryTree a) (BinaryTree a)
--Binary Treeに対して、以下の関数を作って

-- Right fold on a binary tree
foldrTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldrTree = undefined

-- 木をレベルごとリストにする関数。
-- levels $ Node 1 (Node 2 Leaf (Node 3 Leaf Leaf)) (Node 4 (Node 5 Leaf Leaf) (Node 6 (Node 7 Leaf Leaf) Leaf))
-- = [[1],[2,4],[3,5,6], [7]]
levels :: BinaryTree a -> [[a]]
levels = undefined

-- 関数から木を作りましょう
unfoldTree :: (a -> Maybe (n, a, a)) -> a -> BinaryTree n
unfoldTree = undefined

-- Check if a tree has the heap property -> Any node is smaller than all of its children
isHeap :: Ord a => BinaryTree a -> Bool
isHeap = undefined


fix :: (a -> a) -> a
fix f = f $ fix f
--fixは不動点コンビネータです。これを使って再帰を使わない fib, fact などを作って
fib = fix (\fn x -> if x == 1 || x == 2 then 1 else fn (x-1) + fn (x-2))

--グラフのデータ型はどうしたらいいでしょうか
-- data Graph a = ???

