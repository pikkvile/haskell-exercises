module Func2 where

newtype Func r a = Func {run :: (r -> a)}

instance Functor (Func r) where
    fmap f (Func g) = Func $ f . g