{-# LANGUAGE ScopedTypeVariables #-}

module Feb2 where

import Control.Monad.Writer (Writer, tell)
import Control.Exception (handle, IOException)
import System.Environment (getArgs)
import Control.Monad (when)

ffmap :: (a -> b) -> (r -> a) -> (r -> b)
ffmap f g = (\x -> f (g x))

newtype ZL a = ZL {toList :: [a]} deriving Show

instance Functor ZL where
    fmap f xs = ZL (fmap f (toList xs))

instance Applicative ZL where
    pure a = ZL (repeat a)
    fs <*> xs = ZL (zipWith (\f x -> f x) (toList fs) (toList xs))

bindFunc :: (r -> a) -> (a -> r -> b) -> (r -> b)
bindFunc f g = (\x -> g (f x) x)

bindList :: [a] -> (a -> [b]) -> [b]
bindList xs f = concat (map f xs)

main1 :: IO ()
main1 = do
    file <- getLine
    writeFile file "test"

gcd' :: Int -> Int -> Writer [String] Int
gcd' a 0 = do
    tell ["Finished with " ++ show a]
    return a
gcd' a b = do
    let modab = a `mod` b
    tell [show a ++ " mod " ++ show b ++ " == " ++ show modab]
    gcd' b modab

main2 :: IO ()
main2 = handle
    (\(e :: IOException) -> putStrLn "Sorry, something went wrong. probably file does not exist?") $ do
        f <- fmap head getArgs
        c <- readFile f
        putStr c

bindWriter :: Monoid w => W w a -> (a -> W w b) -> W w b
bindWriter (W (w, arg)) f = let W (w', result) = f arg in W (w `mappend` w', result)

newtype W w a = W (w, a) deriving Show

instance (Monoid w) => Functor (W w) where
    fmap f (W (w, a)) = W (w, f a)

instance (Monoid w) => Applicative (W w) where
    pure a = W (mempty, a)
    W (w1, f) <*> W (w2, x) = W (w1 `mappend` w2, f x)

instance (Monoid w) => Monad (W w) where
    m >>= f = bindWriter m f

main3 :: IO ()
main3 = do
    line <- getLine
    when (not (null line)) $ putStrLn (reverse line)

type Pole = (Int, Int)
data Side = L | R
type Landing = (Side, Int)

land :: Landing -> Pole -> Maybe Pole
land (L, n) (l, r)
    | (abs (l + n - r)) < 4 = Just (l + n, r)
    | otherwise = Nothing
land (R, n) (l, r)
    | (abs (l - n - r)) < 4 = Just (l, r + n)
    | otherwise = Nothing

testLandingWithDo :: Pole -> Maybe Pole
testLandingWithDo p = do
    p1 <- land (L, 3) p
    p2 <- land (R, 2) p1
    land (R, 2) p2

main :: IO ()
main = do
    fileName <- getLine
    handle (\(e :: IOException) -> putStrLn "Seems like there's no such file?") (do
        c <- readFile fileName
        putStrLn c)