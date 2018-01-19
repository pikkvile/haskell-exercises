module Jan18 where

newtype ZL a = ZL {toList :: [a]} deriving Show

instance Functor ZL where
    fmap f (ZL xs) = ZL (map f xs)

instance Applicative ZL where
    pure x = ZL (repeat x)
    (ZL fs) <*> (ZL xs) = ZL (zipWith (\f x -> f x) fs xs)

zip3 :: (a -> b -> c -> d) -> ZL a -> ZL b -> ZL c -> ZL d
zip3 f xs ys zs = f <$> xs <*> ys <*> zs

zip3' :: (a -> b -> c -> d) -> ZL a -> ZL b -> ZL c -> ZL d
zip3' f xs ys zs = ((<*>) ((<*>) (fmap f xs) ys) zs)