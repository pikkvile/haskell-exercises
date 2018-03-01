module State where

newtype State' s a = State' { runState :: s -> (a, s) }

instance Functor (State' s) where
    fmap f (State' getX) = State' $ \s -> let (x, s1) = getX s in (f x, s1)

starForState :: (s -> (a -> b, s)) -> (s -> (a, s)) -> (s -> (b, s))
starForState getF getX = \s -> let (f, s1) = getF s
                                   (x, s2) = getX s1 in (f x, s2)

instance Applicative (State' s) where
    pure x = State' $ \s -> (x, s)
    (State' f) <*> (State' x) = State' (starForState f x)

bindForState :: (s -> (a, s)) -> (a -> (s -> (b, s))) -> (s -> (b, s))
bindForState getX f = \s -> let (x, s1) = getX s in f x s1

instance Monad (State' s) where
    (State' f) >>= g = State' $ \s -> let (a, newState) = f s
                                          (State' h) = g a in h newState