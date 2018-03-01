module Stack where

import Control.Monad.State (state, State, get, put)

type Stack = [Int]

push :: Int -> State Stack ()
push i = state $ \s -> ((), i : s)

pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

stackTest :: State Stack Int
stackTest = do
    push 3
    pop
    pop

-- get and put

stackTest2 :: State Stack ()
stackTest2 = do
    stack <- get
    if stack == [1,2,3]
        then put [4,5,6]
        else put [1,2,3]
