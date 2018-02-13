module Jan26 where

import Control.Monad (guard)
import Control.Monad.Writer (tell, Writer)

l1 = [x + 5 | x <- [1..10], odd x]
l2 = [1..10] >>= (\x -> guard (odd x) >> return (x + 5))
l3 = do
    x <- [1..10]
    guard (odd x)
    return (x + 5)

applyWriter :: Monoid m => (a, m) -> (a -> (b, m)) -> (b, m)
applyWriter (value, log) f = let (v', log') = f value in (v', log `mappend` log')

gcd' :: Int -> Int -> Int
gcd' a b
    | b == 0 = a
    | otherwise = gcd' b (a `mod` b)

gcd'' :: Int -> Int -> Writer [String] Int
gcd'' a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        let modab = a `mod` b
        tell [show a ++ " mod " ++ show b ++ " = " ++ show modab]
        gcd'' b modab