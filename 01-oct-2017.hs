-- Sieve of Eratosthenes
sieve :: Int -> [Int]
sieve n = doSieve [2..n]
  where doSieve l@(x:xs) | x * x > n = l
        doSieve (x:xs) = x : doSieve (filter (\n -> n `mod` x /= 0) xs)

-- drop using fold
drop' :: Int -> [a] -> [a]
drop' n = snd . foldl (\(i, ys) x -> if i < n then (i + 1, []) else (i, ys ++ [x])) (0, [])

-- Egyptian multiplication
mlt :: Integral a => a -> a -> a
mlt 1 x = x
mlt n x
  | odd n = x + mlt (n `div` 2) (x + x)
  | even n = mlt (n `div` 2) (x + x)
