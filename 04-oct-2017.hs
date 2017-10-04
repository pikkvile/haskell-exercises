import qualified Data.Map as Map

-- zipWith
zw :: (a -> b -> c) -> [a] -> [b] -> [c]
zw _ [] _ = []
zw _ _ [] = []
zw f (x:xs) (y:ys) = f x y : zw f xs ys

-- Egyptian multiplication
em :: Integral a => a -> a -> a
em 1 x = x
em n x | odd n = x + em (n `div` 2) (x + x)
em n x | even n = em (n `div` 2) (x + x)

-- Either use case
data LockerState = Taken | Free
type Code = String
type Lockers = Map.Map Int (LockerState, Code)
llookup :: Int -> Lockers -> Either String Code
llookup num lockers =
  case Map.lookup num lockers of
    Nothing -> Left $ "locker " ++ show num ++ " does not exists"
    Just (state, code) -> case state of
      Taken -> Left $ "locker " ++ show num ++ " is already taken"
      Free -> Right code

lockers :: Lockers
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]