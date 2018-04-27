module Tree2 where

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

zip' :: Tree a -> Zipper a
zip' t = (t, [])

unzip' :: Zipper a -> Tree a
unzip' (t, _) = t

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Node x l r, cs) = Just (l, LeftCrumb x r : cs)
goLeft (Empty, _) = Nothing

{-goLefts :: Int -> Zipper a -> Zipper a
goLefts n = foldr (.) id (replicate n goLeft)-}

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