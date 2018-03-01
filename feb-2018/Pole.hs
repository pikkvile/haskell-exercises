module Pole where

import Control.Monad.Error

type Pole = (Int, Int)

data Landing = LL Int | LR Int

data PoleError = PoleError Pole
instance Show PoleError where
    show (PoleError (l,r)) = "Fail: left = " ++ show l ++ ", right: " ++ show r
instance Error PoleError

land :: Landing -> Pole -> Either PoleError Pole
land (LL n) (l, r)
    | (l + n - r) < 4 = Right (l + n, r)
    | otherwise = Left (PoleError (l + n, r))
land (LR n) (l, r)
    | (l - n - r) < 4 = Right (l, r + n)
    | otherwise = Left (PoleError (l, r + n))

