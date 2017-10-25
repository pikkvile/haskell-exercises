-- drop using fold (bad, just for exercise)
drp :: Int -> [a] -> [a]
drp n = fst . foldl (\(lst, c) x -> if c < n then (lst, c + 1) else (lst ++ [x], c)) ([], 0)

-- fibonacci
f :: Int -> [Integer]
f i = ff [1, 0]
  where
    ff :: [Integer] -> [Integer]
    ff l | length l == i = l
    ff l@(x:y:_) = ff $ (x + y) : l