module Feb where

import Control.Monad (guard)
import Control.Monad.Writer (tell, Writer)
import Control.Applicative (liftA2)
import System.IO (writeFile, withFile, IOMode (ReadMode), hGetContents)

l1 = [x + 2 | x <- [1,4,5,6], x > 2]
l2 = [1,4,5,6] >>= (\x -> guard (x > 2) >> [x + 2])
l3 = do
    x <- [1,4,5,6]
    guard (x > 2)
    return (x + 2)

gcd' :: Int -> Int -> Writer [String] Int
gcd' a 0 = do
    tell ["Done with " ++ show a]
    return a
gcd' a b = do
    let abmod = a `mod` b
    tell [show a ++ " mod " ++ show b ++ " = " ++ show abmod]
    gcd' b abmod

main1 :: IO ()
main1 = do
    conc <- (++) <$> getLine <*> getLine
    putStrLn conc

bindForFunction :: (r -> a) -> (a -> (r -> b)) -> (r -> b)
bindForFunction f g = \x -> g (f x) x

readerInAction :: Int -> Int
readerInAction = do
    a <- (+2)
    b <- (*2)
    return (a + b)

sequenceA' :: Applicative f => [f a] -> f [a]
sequenceA' [] = pure []
--sequenceA' (x:xs) = (:) <$> x <*> sequenceA' xs
-- ((<$>) (:) x) make a function out of x; x is an applicative value... something wrapper inside applicative box.
-- (:) is a function which takes type wrapped inside applicative.
-- <$> is an fmap, it takes a function and apply it to value inside a functor. Since applicative is functior, this is
-- possible. So we have function (:) applied to value inside x wrapper in same Applicative. And in this case this result
-- is a function itself (cause (:) takes two args, it was applied partially). Infix form: <$> (:) x
sequenceA' (x:xs) = (<*>) ((<$>) (:) x) (sequenceA' xs)

sequenceA'' :: Applicative f => [f a] -> f [a]
--sequenceA'' = foldr (\x acc -> (:) <$> x <*> acc) (pure [])
sequenceA'' = foldr (liftA2 (:)) (pure [])

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing _ = Nothing
applyMaybe (Just x) f = f x

main2 :: IO ()
main2 = do
    path <- getLine
    content <- getLine
    writeFile path content

main3 :: IO ()
main3 = do
    path <- getLine
    contents <- readFile path
    putStrLn contents

fmapForFunction :: (a -> b) -> (r -> a) -> (r -> b)
fmapForFunction f g = f . g

fmapForFunction' :: (a -> b) -> (r -> a) -> (r -> b)
fmapForFunction' f g = (\x -> f (g x))

starForFunction :: (r -> (a -> b)) -> (r -> a) -> (r -> b)
starForFunction f g = \x -> f x (g x)

main4 :: IO ()
main4 = do
    file <- getLine
    withFile file ReadMode (\h -> do
        c <- hGetContents h
        putStr ("Contents:\n" ++ c))

doRPN :: String -> String
doRPN expr = show . head $ foldl process [] (words expr)
    where
        process :: [Double] -> String -> [Double]
        process (a : b : tail) "+" = (a + b) : tail
        process (b : a : tail) "-" = (a - b) : tail
        process (a : b : tail) "*" = (a * b) : tail
        process (b : a : tail) "/" = (a / b) : tail
        process stack num = (read num) : stack