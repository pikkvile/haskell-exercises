module Pole where

data Landing = L Int | R Int

type Pole = (Int, Int)

land :: Landing -> Pole -> Maybe Pole
land (L n) (l, r)
    | abs (l + n - r) < 4 = Just (l + n, r)
    | otherwise = Nothing
land (R n) (l, r)
    | abs (l - n - r) < 4 = Just (l, r + n)
    | otherwise = Nothing

-- todo error handling proper