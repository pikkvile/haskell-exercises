module Ankis where

import Prelude hiding (sequenceA)
import Control.Applicative
import Control.Monad

sequenceAr :: Applicative f => [f a] -> f [a]
sequenceAr [] = pure []
sequenceAr (x:xs) = (:) <$> x <*> (sequenceAr xs)

sequenceAf :: Applicative f => [f a] -> f [a]
sequenceAf = foldr (liftA2 (:)) (pure [])

main1 = do
    inputs <- sequenceAr [getLine, getLine]
    putStrLn (show (map (\l -> "OK! " ++ l) inputs))

main2 = do
    conc <- liftA2 (++) getLine getLine
    putStrLn conc

main3 = do
    inputs <- sequence [getLine, getLine]
    putStrLn (foldl (++) "" inputs)

newtype ZL a = ZL {toList :: [a]} deriving Show

instance Functor ZL where
    fmap f (ZL xs) = ZL (map f xs)

instance Applicative ZL where
    pure x = ZL (repeat x)
    ZL fs <*> ZL xs = ZL (zipWith (\f x -> f x) fs xs)