import qualified Data.Map as Map

-- negate
ng :: [Int] -> [Int]
ng = map (negate . abs)

-- product
p :: (Num a, Foldable t) => t a -> a
p = foldl (*) 1

-- How many elements does it take for the sum of the roots of all natural numbers to exceed 1000? Using scanl
hm :: Int
hm = 1 + (length $ takeWhile (<1000) (scanl (+) 0 (map sqrt [1..])))

-- seive of Eratosthenes (find all primes less than)
es :: Int -> [Int]
es n = 1 : doSeive [2..n]
  where
    doSeive :: [Int] -> [Int]
    doSeive primes@(x:_) | x * x >= n = primes
    doSeive (x:xs) = x : doSeive (filter (\e -> e `mod` x /= 0) xs)

-- locker lookups
data Locker = Locker LockerState Code deriving Show
data LockerState = Free | Taken deriving Show
type Code = String
type Lockers = Map.Map Int Locker

lockers :: Lockers
lockers = Map.fromList [
    (1, (Locker Free "asdasd")),
    (2, (Locker Free "sdfsdfsdf")),
    (3, (Locker Taken "sdfsdsdfdsdf"))]

lookup' :: Int -> Either String Code
lookup' n = case Map.lookup n lockers of
  Nothing -> Left "Sorry, no such locker"
  Just (Locker state code) -> case state of
    Taken -> Left "Sorry, locker already taken"
    Free -> Right code

