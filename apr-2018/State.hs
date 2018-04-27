module State where

newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
    fmap f (State sc) = State $ \s -> let (x, s') = sc s in (f x, s')