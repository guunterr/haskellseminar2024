-- data Vertex a = Vertex Int a
--     deriving (Show, Eq, Read)
-- data Edge = Edge NodeID Weight
--     deriving (Show, Eq, Read)
-- type Weight = Int
-- type NodeID = Int

-- newtype Graph a = Graph [(Vertex a, [Edge])]
--     deriving (Show)

--Student -> Name and Department and StudentID and DOB
--Student -> String and String and Integer and String
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

import Prelude hiding (map,length,minimum, concat)
type Name = String
data Department = Informatics | Science | Law
    deriving (Show)
type StudentID = Integer
data DateOfBirth = DOB Year Month Day
    deriving (Show)
type Year = Integer
type Month = Integer
type Day = Integer 

data Student = S Name Department StudentID DateOfBirth
    deriving (Show)

gerard :: Student
gerard = S "Gerard Glowacki" Informatics 102210088 (DOB 2002 4 5)

data List a = Nil | Cons a (List a)
    deriving (Show)

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap f Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
  pure :: a -> List a
  pure a = Cons a Nil
  (<*>) :: List (a -> b) -> List a -> List b
  -- [f,g] <*> [a,b,c] = [f a, f b, f c, g a, g b, g c]
  (<*>) Nil as = Nil
  (<*>) (Cons f fs) as = plusplus (fmap f as) ((<*>) fs as)

instance Monad List where

    (>>=) :: List a -> (a -> List b) -> List b
    (>>=) Nil f = Nil
    (>>=) (Cons a as) f = plusplus (f a) ((>>=) as f)
    
join' :: List (List a) -> List a
join' as = (>>=) as id


toList :: List a -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs

fromList :: [a] -> List a
fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)

length :: List a -> Integer
length = undefined

map :: (a -> b) -> List a -> List b
map = fmap

minimum :: Ord a => List a -> a
minimum = undefined

plusplus :: List a -> List a -> List a
-- plusplus [1,2,3] [4,5,6] = [1,2,3,4,5,6]
plusplus xs ys = fromList $ toList xs ++ toList ys

concat :: List (List a) -> List a
-- concat [[1,2,3],[4,5,6],[7,8,9]] = [1,2,3,4,5,6,7,8,9]
concat = undefined

testList1 :: List Integer
testList1 = Cons 1 $ Cons 2 $ Cons 3 $ Nil
testList2 :: List Integer
testList2 = Cons 4 $ Cons 5 $ Cons 6 $ Nil

testList3 :: List (Integer -> Integer)
testList3 = Cons (flip (-) 4) (Cons (+4) Nil)

testFn1 :: Integer -> List Integer
testFn1 n = Cons n $ Cons (n+1) $ Cons (n+2) Nil


