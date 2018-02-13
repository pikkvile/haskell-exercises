module Jan29 where

import Data.Char (toUpper)
import Prelude hiding (Left, Right)

main = getLine >>= (\l -> putStrLn (map toUpper l))

type Birds = Int
type Pole = (Birds, Birds)
data Side = Left | Right
type Landing = (Side, Birds)

land :: Landing -> Pole -> Maybe Pole
land (Left, n) (l, r)
    | abs (l + n - r) < 4 = Just (l + n, r)
    | otherwise = Nothing
land (Right, n) (l, r)
    | abs (l - r - n) < 4 = Just (l, r + n)
    | otherwise = Nothing

data Tree a = E | N a (Tree a) (Tree a) deriving Show

instance Functor Tree where
    fmap _ E = E
    fmap f (N v l r) = N (f v) (fmap f l) (fmap f r)

instance Foldable Tree where
    foldMap _ E = mempty
    foldMap f (N v l r) = (foldMap f l) `mappend` (f v) `mappend` (foldMap f r)

applyList :: [a] -> (a -> [b]) -> [b]
applyList [] _ = []
applyList xs f = concat (map f xs)