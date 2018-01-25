module Jan22 where

import Control.Monad (guard)

-- birds

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
    | abs (left + n - right) < 4 = Just (left + n, right)
    | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
    | abs (left - n - right) < 4 = Just (left, right + n)
    | otherwise = Nothing

-- applyMaybe

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing _ = Nothing
applyMaybe (Just x) f = f x

-- list comprehension, guard, guard in do

fltr :: Int -> Int -> Bool
fltr x y = ('7' `elem` show x) && ('6' `elem` show y) && (x + y) `mod` 3 == 0

filtered  = [x + y | x <- [1..50], y <- [1..50], fltr x y]
filtered1 = [1..50] >>= (\x -> (map (\y -> (x,y)) [1..50])) >>= (\(x,y) -> guard (fltr x y) >> return (x + y))
filtered2 = do
    x <- [1..50]
    y <- [1..50]
    guard (fltr x y)
    return (x + y)