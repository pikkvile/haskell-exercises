module FilterM where

import Control.Monad (filterM)

powerset :: [a] -> [[a]]
powerset = filterM (\x -> [True, False])