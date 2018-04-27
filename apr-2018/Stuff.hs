module Stuff where

newtype Fn r a = Fn {run :: r -> a}

instance Functor (Fn r) where
    fmap f (Fn g) = Fn $ f . g