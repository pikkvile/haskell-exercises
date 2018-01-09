module Tree where

import qualified Data.Foldable as F

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving Show

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

instance F.Foldable Tree where
    foldMap f EmptyTree = mempty
    foldMap f (Node x left right) = (foldMap f left) `mappend` f x `mappend` (foldMap f right)