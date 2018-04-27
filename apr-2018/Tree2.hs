module Tree2 where

import Data.Foldable (Foldable)

data Tree a = Empty | Node a (Tree a) (Tree a)

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Node x l r) = (Node (f x) (fmap f l) (fmap f r))

instance Foldable Tree where
    foldMap _ Empty = mempty
    foldMap f (Node x l r) = (foldMap f l) `mappend` (f x) `mappend` (foldMap f r)