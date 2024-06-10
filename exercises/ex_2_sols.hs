--Rose Tree : ノードには任意の子の数のあるツリー
data RoseTree a = RoseLeaf | RoseNode a [RoseTree a]
-- 型を定義し、mapを作って


mapRose :: (a -> b) -> RoseTree a -> RoseTree b
mapRose f RoseLeaf = RoseLeaf
mapRose f (RoseNode a ts) = RoseNode (f a) (map (mapRose f) ts)


data BinaryTree a = Leaf | Node a (BinaryTree a) (BinaryTree a)
    deriving(Show)

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