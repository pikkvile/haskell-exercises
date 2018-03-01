module ListZipper where

type ListZipper a = ([a], [a])

start :: [a] -> ListZipper a
start xs = (xs, [])

toStart :: ListZipper a -> ListZipper a
toStart z@(xs, []) = z
toStart z = toStart $ goBack z

collect :: ListZipper a -> [a]
collect = fst . toStart

goForward :: ListZipper a -> ListZipper a
goForward (x:xs, cs) = (xs, x:cs)

goForwardN :: Int -> ListZipper a -> ListZipper a
goForwardN n = foldr (.) id (replicate n goForward)

goBack :: ListZipper a -> ListZipper a
goBack (xs, x:cs) = (x:xs, cs)

goBackN :: Int -> ListZipper a -> ListZipper a
goBackN n = foldr (.) id (replicate n goBack)

modify :: (a -> a) -> ListZipper a -> ListZipper a
modify f (x:xs, cs) = ((f x):xs, cs)

append :: [a] -> ListZipper a -> ListZipper a
append ys (xs, cs) = (ys, cs)

(-:) :: a -> (a -> b) -> b
(-:) x f = f x