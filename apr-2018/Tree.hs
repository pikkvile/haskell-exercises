module Tree where

import Data.Foldable (Foldable)

data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

instance Functor Tree where
    fmap f Empty = Empty
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

instance Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Node x l r) = (foldMap f l) `mappend` f x `mappend` (foldMap f r)

tree :: Tree String
tree = Node
        "A"
        (Node
            "B"
            (Node "C" Empty Empty)
            (Node "D" Empty Empty))
        (Node
            "E"
            Empty
            (Node "F" Empty Empty))

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving Show
type Crumbs a = [Crumb a]
type TreeZipper a = (Tree a, Crumbs a)

asTop :: Tree a -> TreeZipper a
asTop t = (t, [])

goLeft :: TreeZipper a -> Maybe (TreeZipper a)
goLeft (Node _ Empty _, _) = Nothing
goLeft (Node x l r, cs) = return (l, LeftCrumb x r : cs)

goRight :: TreeZipper a -> Maybe (TreeZipper a)
goRight (Node _ _ Empty, _) = Nothing
goRight (Node x l r, cs) = return (r, RightCrumb x l : cs)

up :: TreeZipper a -> TreeZipper a
up (t, (LeftCrumb x r):cs) = (Node x t r, cs)
up (t, (RightCrumb x l):cs) = (Node x l t, cs)

goUp :: TreeZipper a -> Maybe (TreeZipper a)
goUp (_, []) = Nothing
goUp z = return (up z)

goTop :: TreeZipper a -> TreeZipper a
goTop z@(_, []) = z
goTop z = goTop (up z)