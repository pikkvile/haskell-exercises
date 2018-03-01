module Pole2 where

type Pole = (Int, Int)
data Landing = LL Int | LR Int

land :: Landing -> Pole -> Maybe Pole
land (LL n) (l, r)
    | abs (l + n - r) < 4 = Just (l + n, r)
    | otherwise = Nothing
land (LR n) (l, r)
    | abs (l - n - r) < 4 = Just (l, r + n)
    | otherwise = Nothing

