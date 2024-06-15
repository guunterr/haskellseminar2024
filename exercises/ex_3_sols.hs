--Define Functor, Applicative and Monad instances for the following two types:
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fold" #-}

import Prelude hiding (Just, Nothing, Monoid, mempty, mappend, mconcat, Maybe, (<>))

data Maybe a = Nothing | Just a
  deriving (Show, Read, Eq)
data BinaryTree a = Leaf | Node a (BinaryTree a) (BinaryTree a)
  deriving (Show, Read, Eq)

instance Functor Maybe where
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap f Nothing = Nothing
  fmap f (Just x) = Just (f x)

-- f: A -> B ==> F f : F A -> F B 
--fmap id = id :: Maybe a
--fmap (f . g) = (fmap f) . (fmap g)

instance Applicative Maybe where
  pure :: a -> Maybe a
  pure = Just
  (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  (<*>) Nothing x = Nothing
  (<*>) f Nothing = Nothing
  (<*>) (Just f) (Just x) = Just (f x)

-- pure id <*> v = v                            -- Identity
-- pure f <*> pure x = pure (f x)               -- Homomorphism
-- u <*> pure y = pure ($ y) <*> u              -- Interchange
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w) -- Composition

instance Monad Maybe where
  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  (>>=) Nothing f = Nothing
  (>>=) (Just x) f = f x


-- Monoidal object ==> join :: Maybe Maybe a -> Maybe a  (>>=) = join . pure
-- k :: a -> m a
-- pure a   >>= k                  =  k a
-- m        >>= pure               =  m
-- m        >>= (\x -> k x >>= h)  =  (m >>= k) >>= h

instance Functor BinaryTree where
  fmap :: (a -> b) -> BinaryTree a -> BinaryTree b
  fmap f Leaf = Leaf
  fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)

--ネタバレ：うまくいかない
instance Applicative BinaryTree where
  pure :: a -> BinaryTree a
  pure = undefined
  (<*>) :: BinaryTree (a -> b) -> BinaryTree a -> BinaryTree b
  (<*>) = undefined

--ネタバレ：うまくいかない
instance Monad BinaryTree where
  (>>=) :: BinaryTree a -> (a -> BinaryTree b) -> BinaryTree b
  (>>=) = undefined

data RoseTree a = RoseLeaf | RoseTree a [RoseTree a]
data LeafyTree a = LeafyLeaf a | LeafyNode (LeafyTree a) (LeafyTree a)
-- Finger Tree -> almost all operations amortized O(1)

--Those who are more categorically inclined can also prove that these implementations are actually correct
--e.g fmap id == id, fmap (f . g) == fmap f . fmap g, same for pure, <*> ,>>= etc

--This is a monoid

class Monoid m where
  mempty :: m
  (<>) :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr (<>) mempty

--Also create the following instance for list

instance Monoid [a] where
  mempty :: [a]
  mempty  = []
  (<>) :: [a] -> [a] -> [a]
  (<>) = (++)


-- fというのは, x -> f x
--Then write the following more general version of the function from last weeks exercises

zipConcat :: [[a]] -> [[a]] -> [[a]]
zipConcat [] [] = []
zipConcat (x:xs) (y:ys) = (x ++ y) : zipConcat xs ys
zipConcat xs [] = xs
zipConcat [] ys = ys

zipMonoid :: Monoid m => [m] -> [m] -> [m]
zipMonoid [] [] = [mempty]
zipMonoid (x:xs) (y:ys) = (<>) x y : zipMonoid xs ys
zipMonoid xs [] = xs
zipMonoid [] ys = ys