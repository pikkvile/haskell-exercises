module Func where

newtype Func r a = Func {getF :: r -> a}

instance Functor (Func r) where
    fmap f (Func g) = Func (f . g)

instance Applicative (Func r) where
    pure a = Func (\_ -> a)
    (Func f) <*> (Func g) = Func (\x -> (f x) (g x))