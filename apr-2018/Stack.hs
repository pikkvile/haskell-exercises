module Stack where

import Control.Monad.State (State, state)

type Stack a = [a]

put :: a -> State (Stack a) ()
put x = state $ \s -> ((), x : s)

pop :: State (Stack a) a
pop = state $ \(top : s) -> (top, s)

stackAdd = do
    x <- pop
    y <- pop
    return (x + y)