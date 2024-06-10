

type Year = Int
type Month = Int
type Day = Int
data DOB = Date Year Month Day
    deriving (Show)

data Student = S Int String DOB
data Teacher = T Int String DOB
data Staff = ST Int String DOB

data UniversityPerson = Foo Student | Bar Teacher | Biz Staff

gerard :: Student
gerard = S 102210088 "Gerard Glowacki" (Date 2002 04 05)

showStudent :: Student -> String
showStudent (S number name date) = show number ++ show name ++ show date

data Product a b = Product a b
first :: Product t1 t2 -> t1
first (Product a b) = a

second :: Product t1 t2 -> t2
second (Product a b) = b

prodUniversalProperty :: (c -> a) -> (c -> b) -> c -> Product a b
prodUniversalProperty fa fb c = Product (fa c) (fb c)

data Coproduct a b = Hidari a | Migi b
coprodUniversalProperty :: (a -> c) -> (b -> c) -> Coproduct a b -> c
coprodUniversalProperty fa fb (Hidari a) = fa a
coprodUniversalProperty fa fb (Migi b) = fb b

data Kamosen a = Nanimonai | Nankaaru a
    deriving(Show)
(!?) :: [Int] -> Int -> Kamosen Int
(!?) xs n
    | length xs <= n    = Nanimonai
    | otherwise         = Nankaaru $ xs !! n

data List a = Nil | Cons a (List a) -- [] | x : xs
length' :: List a -> Int
length' Nil = 0
length' (Cons x xs) = 1 + length' xs

take' :: Int -> List a -> List a
take' n Nil = Nil
take' 0 xs = Nil
take' n (Cons x xs) = Cons x $ take' (n-1) xs

map' :: (a -> b) -> List a -> List b
map' f Nil = Nil
map' f (Cons x xs) = Cons (f x) (map' f xs)

data BinaryTree a = Leaf | Node a (BinaryTree a) (BinaryTree a)
mapT :: (a -> b) -> BinaryTree a -> BinaryTree b
mapT f Leaf = Leaf
mapT f (Node a t1 t2) = Node (f a) (mapT f t1) (mapT f t2)

dfs :: Eq a => a -> BinaryTree a -> Bool
dfs x Leaf = False
dfs x (Node a t1 t2) = (x == a) || dfs x t1 || dfs x t2

type Queue a = [a]
enqueue :: Queue a -> a -> Queue a
enqueue q a = q ++ [a]

dequeue :: Queue a -> Maybe a
dequeue [] = Nothing
dequeue (x:xs) = Just x






