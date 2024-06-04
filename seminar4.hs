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

fromList :: [a] -> List a
fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)

length :: List a -> Integer
length = undefined

map :: (a -> b) -> List a -> List b
map = undefined

minimum :: Ord a => List a -> a
minimum = undefined

plusplus :: List a -> List a -> List a
-- plusplus [1,2,3] [4,5,6] = [1,2,3,4,5,6]
plusplus = undefined

concat :: List (List a) -> List a
-- concat [[1,2,3],[4,5,6],[7,8,9]] = [1,2,3,4,5,6,7,8,9]
concat = undefined




