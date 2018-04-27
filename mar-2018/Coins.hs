module Coins where

import Data.Ratio

data CoinSide = Head | Tail deriving Show

newtype PList a = PList {getList :: [(a, Rational)]} deriving Show

instance Functor PList where
    fmap f (PList xps) = PList $ map (\(x, p) -> (f x, p)) xps

instance Applicative PList where
    pure x = PList $ [(x, 1)]
    (PList fps) <*> (PList xps) = PList $ [(f x, px * pf) | (f, pf) <- fps, (x, px) <- xps]

instance Monad PList where
    xps >>= f = PList $ concat $ map adjustProbability (getList (fmap f xps))
        where
            adjustProbability :: (PList a, Rational) -> [(a, Rational)]
            adjustProbability (PList xps, p) = map (\(x, px) -> (x, px * p)) xps

