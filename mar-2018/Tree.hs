module Tree where

data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

testTree = Node 1
             (Node 2
               (Node 3 Empty Empty)
               (Node 4 Empty Empty))
             (Node 5
                Empty
                (Node 6
                    (Node 7 Empty Empty)
                    Empty))

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving Show
type Zipper a = (Tree a, [Crumb a])

goLeft :: Zipper a -> Zipper a
goLeft (Node x l r, cs) = (l, LeftCrumb x r : cs)

goRight :: Zipper a -> Zipper a
goRight (Node x l r, cs) = (r, RightCrumb x l : cs)

goUp :: Zipper a -> Zipper a
goUp (t, LeftCrumb x r : cs) = (Node x t r, cs)
goUp (t, RightCrumb x l : cs) = (Node x l t, cs)

goRoot :: Zipper a -> Zipper a
goRoot z@(root, []) = z
goRoot z = goRoot . goUp $ z

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, cs) = (Node (f x) l r, cs)

attach :: Tree a -> Zipper a -> Zipper a
attach t (_, cs) = (t, cs)

(-:) :: a -> (a -> b) -> b
x -: f = f x