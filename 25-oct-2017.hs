-- filter
f :: (a -> Bool) -> [a] -> [a]
f _ [] = []
f p (x:xs) = if p x then x : f p xs else f p xs

-- data 2d Vector
data V a = V a a deriving Show

-- sum two vectors
vp :: Num a => V a -> V a -> V a
vp (V x1 y1) (V x2 y2) = V (x1 + x2) (y1 + y2)

-- multiply scalar on vector
vsm :: Num a => a -> V a -> V a
vsm s (V x y) = V (s * x) (s * y)

-- scalar multiplication
vm :: Num a => V a -> V a -> a
vm (V x1 y1) (V x2 y2) = x1 * x2 + y1 * y2

-- replicate
r :: Int -> a -> [a]
r 0 _ = []
r n a = a : r (n - 1) a