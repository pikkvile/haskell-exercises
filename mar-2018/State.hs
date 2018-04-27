module State where

newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
    fmap f (State sc) = State $ \s -> let (x, s') = sc s in (f x, s')

instance Applicative (State s) where
    pure x = State $ \s -> (x, s)
    (State scf) <*> (State scx) = State $ \s -> let (f, s') = scf s
                                                    (x, s'') = scx s' in (f x, s'')

instance Monad (State s) where
    (State sc) >>= f = State $ \s -> let (x, s') = sc s
                                         (State sc') = f x in sc' s'