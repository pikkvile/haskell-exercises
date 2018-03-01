module Stack3 where

import Control.Monad.State (State, state)

type Stack a = [a]

push :: a -> State (Stack a) ()
push x = state $ \s -> ((), x:s)

pop :: State (Stack a) a
pop = state $ \(x:xs) -> (x, xs)