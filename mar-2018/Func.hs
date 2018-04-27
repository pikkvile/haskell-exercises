module Func where

newtype F r a = F {apply :: r -> a}

instance Functor (F r) where
    fmap f (F g) = F $ f . g

instance Applicative (F r) where
    pure x = F $ (\_ -> x)
    (F f) <*> (F g) = F $ (\x -> let f' = f x
                                     y = g x in f' y)

instance Monad (F r) where
    (F f) >>= g = F $ \r -> let (F g') = g (f r) in g' r

inAction :: F Int Int
inAction = do
    a <- F (+2)
    b <- F (*2)
    return (a + b)