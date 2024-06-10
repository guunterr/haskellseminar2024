--Define Functor, Applicative and Monad instances for the following two types:
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fold" #-}

import Prelude hiding (Monoid, mempty, mappend, mconcat, Maybe)

data Maybe a = Nothing | Just a
  deriving (Show, Read, Eq)
data BinaryTree a = Leaf | Node a (BinaryTree a) (BinaryTree a)
  deriving (Show, Read, Eq)

instance Functor Maybe where  
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap = undefined

instance Applicative Maybe where
  pure :: a -> Maybe a
  pure = undefined
  (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  (<*>) = undefined

instance Monad Maybe where
  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  (>>=) = undefined

instance Functor BinaryTree where  
  fmap :: (a -> b) -> BinaryTree a -> BinaryTree b
  fmap = undefined

instance Applicative BinaryTree where
  pure :: a -> BinaryTree a
  pure = undefined
  (<*>) :: BinaryTree (a -> b) -> BinaryTree a -> BinaryTree b
  (<*>) = undefined

instance Monad BinaryTree where
  (>>=) :: BinaryTree a -> (a -> BinaryTree b) -> BinaryTree b
  (>>=) = undefined

--Those who are more categorically inclined can also prove that these implementations are actually correct
--e.g fmap id == id, fmap (f . g) == fmap f . fmap g, same for pure, <*> ,>>= etc

--This is a monoid

class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty

--Also create the following instance for list

instance Monoid [a] where
  mempty :: [a]
  mempty = undefined
  mappend :: [a] -> [a] -> [a]
  mappend = undefined
  
--Then write the following more general version of the function from last weeks exercises

zipConcat :: [[a]] -> [[a]] -> [[a]]
zipConcat [] [] = []
zipConcat (x:xs) (y:ys) = (x ++ y) : zipConcat xs ys
zipConcat xs [] = xs
zipConcat [] ys = ys

zipMonoid :: Monoid m => [m] -> [m] -> [m]
zipMonoid = undefined