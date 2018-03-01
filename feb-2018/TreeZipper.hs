module TreeZipper where

data Tree a = Empty | Node a (Tree a) (Tree a)
data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a)
type Crumbs a = [Crumb a]
type Zipper a = (Tree a, Crumbs a)

goLeft :: Zipper a -> Zipper a
goLeft (Node x l r, cs) = (l, LeftCrumb x r : cs)

goRight :: Zipper a -> Zipper a
goRight (Node x l r, cs) = (r, RightCrumb x l : cs)

goUp :: Zipper a -> Zipper a
goUp (t, LeftCrumb x r : cs) = (Node x t r, cs)
goUp (t, RightCrumb x l : cs) = (Node x l t, cs)

topMost :: Zipper a -> Zipper a
topMost z@(_, []) = z
topMost z = topMost (goUp z)

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify _ z@(Empty, _) = z

attach :: Tree a -> Zipper a -> Zipper a
attach t (_, cs) = (t, cs)