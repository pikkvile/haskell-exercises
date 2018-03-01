module Prob where

import Data.Ratio (Rational, (%))

newtype PList a = PList {getPList :: [(a, Rational)]} deriving Show

instance Functor PList where
    fmap f (PList xs) = PList $ map (\(x, p) -> (f x, p)) xs

instance Applicative PList where
    pure x = PList [(x,1)]
    PList fs <*> PList xs = PList [(f x, pf * px) | (f, pf) <- fs, (x, px) <- xs]

flatten :: PList (PList a) -> PList a
flatten (PList xsp) = PList $ concat $ map mult xsp
    where
        mult :: (PList a, Rational) -> [(a, Rational)]
        mult (PList xs, p) = map (\(x, px) -> (x, px * p)) xs

instance Monad PList where
    pxs >>= f = flatten (fmap f pxs)

data CoinSide = Head | Tail deriving (Show, Eq)

dropCoin :: PList CoinSide
dropCoin = PList [(Head, 1%2), (Tail, 1%2)]

dropThree :: PList [CoinSide]
dropThree = do
    one <- dropCoin
    two <- dropCoin
    three <- dropCoin
    return [one, two, three]