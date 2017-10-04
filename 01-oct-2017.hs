-- Sieve of Eratosthenes
sieve :: Int -> [Int]
sieve n = 1 : doSieve [2..n]
  where
    doSieve :: [Int] -> [Int]
    doSieve primes@(x:xs) | x * x > n = primes
    doSieve (x:xs) = x : doSieve (filter (\i -> i `mod` x /= 0) xs)

-- drop using fold
drop' :: Int -> [a] -> [a]
drop' n = snd . foldl (\(i, ys) x -> if i < n then (i + 1, []) else (i, ys ++ [x])) (0, [])

-- Egyptian multiplication
mlt :: Integral a => a -> a -> a
mlt 1 x = x
mlt n x
  | odd n = x + mlt (n `div` 2) (x + x)
  | even n = mlt (n `div` 2) (x + x)

-- Vector type
data Vector a = Vector a a a deriving (Show)

vplus :: Num t => Vector t -> Vector t -> Vector t
(Vector x1 y1 z1) `vplus` (Vector x2 y2 z2) = Vector (x1 + x2) (y1 + y2) (z1 + z2)

vmult :: Num t => Vector t -> t -> Vector t
(Vector x y z) `vmult` n = Vector (x * n) (y * n) (z * n)

scalarmult :: Num t => Vector t -> Vector t -> t
(Vector x1 y1 z1 ) `scalarmult` (Vector x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

-- recursive elem
elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs) = e == x || elem' e xs

-- Day type
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)