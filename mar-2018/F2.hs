module F2 where

newtype F r a = F {run :: r -> a}

instance Functor (F r) where
    fmap f (F g) = F $ f . g

instance Applicative (F r) where
    pure x = F $ \_ -> x
    F f <*>  F g = F $ \x -> (f x) (g x)

instance Monad (F r) where
    F f >>= g = F $ \x -> let (F h) = g (f x) in h x


test = do
    a <- (+2)
    b <- (*3)
    return (a + b)