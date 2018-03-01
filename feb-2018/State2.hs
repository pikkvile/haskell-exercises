module State2 where

newtype S2 s a = S2 {runState :: s -> (a, s)}

instance Functor (S2 s) where
    fmap f (S2 comp) = S2 (\s -> let (x, newState) = comp s in (f x, newState))

instance Applicative (S2 s) where
    pure a = S2 (\s -> (a, s))
    S2 compF <*> S2 compX = S2 (\s -> let (f, s1) = compF s
                                          (x, s2) = compX s1 in (f x, s2))

instance Monad (S2 s) where
    S2 comp >>= f = S2 (\s -> let (x, s1) = comp s
                                  (S2 g) = f x in g s1)