--Define Functor, Applicative and Monad instances for the following two types:
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fold" #-}

import Prelude hiding (Just, Nothing, Monoid, mempty, mappend, mconcat, Maybe)

data Maybe a = Nothing | Just a
  deriving (Show, Read, Eq)
data BinaryTree a = Leaf | Node a (BinaryTree a) (BinaryTree a)
  deriving (Show, Read, Eq)

instance Functor Maybe where
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap f Nothing = Nothing
  fmap f (Just x) = Just $ f x

instance Applicative Maybe where
  pure :: a -> Maybe a
  pure = Just
  (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  (<*>) Nothing _ = Nothing
  (<*>) (Just f) Nothing = Nothing
  (<*>) (Just f) (Just a) = Just $ f a

instance Monad Maybe where
  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  (>>=) Nothing _ = Nothing
  (>>=) (Just a) f = f a

instance Functor BinaryTree where
  fmap :: (a -> b) -> BinaryTree a -> BinaryTree b
  fmap f Leaf = Leaf
  fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)

instance Applicative BinaryTree where
  pure :: a -> BinaryTree a
  pure a = Node a (pure a) (pure a)
  (<*>) :: BinaryTree (a -> b) -> BinaryTree a -> BinaryTree b
  (<*>) _ Leaf = Leaf
  (<*>) Leaf _ = Leaf
  (<*>) (Node f fl fr) (Node a l r) = Node (f a) Leaf Leaf

instance Monad BinaryTree where
  (>>=) :: BinaryTree a -> (a -> BinaryTree b) -> BinaryTree b
  (>>=) Leaf _ = Leaf
  (>>=) (Node a l r) f = f a

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
  mempty = []
  mappend :: [a] -> [a] -> [a]
  mappend = (++)

--Then write the following more general version of the function from last weeks exercises

zipConcat :: [[a]] -> [[a]] -> [[a]]
zipConcat [] [] = []
zipConcat (x:xs) (y:ys) = (x ++ y) : zipConcat xs ys
zipConcat xs [] = xs
zipConcat [] ys = ys

zipMonoid :: Monoid m => [m] -> [m] -> [m]
zipMonoid [] [] = []
zipMonoid (x:xs) (y:ys) = mappend x y : zipMonoid xs ys
zipMonoid xs [] = xs
zipMonoid [] ys = ys