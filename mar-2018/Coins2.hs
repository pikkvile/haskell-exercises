module Coins2 where

import Data.Ratio (Rational, (%))

newtype PList a = PList [(a, Rational)] deriving Show

instance Functor PList where
    fmap f (PList xps) = PList $ map (\(x, p) -> (f x, p)) xps

instance Applicative PList where
    pure x = PList $ [(x, 1)]
    PList fps <*> PList xps = PList $ [(f x, pf * px) | (f, pf) <- fps, (x, px) <- xps]

instance Monad PList where
    PList xps >>= fm = PList $ concat $ map (\(x, px) -> let (PList yps) = fm x in map (\(y, py) -> (y, py * px)) yps) xps

data CoinSide = One | Another deriving Show

dropCoin :: PList CoinSide
dropCoin = PList $ [(One, 1%2), (Another, 1%2)]

test = do
    d1 <- dropCoin
    d2 <- dropCoin
    return [d1, d2]