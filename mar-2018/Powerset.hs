module Powerset where

import Control.Monad (filterM)

powerset :: [a] -> [[a]]
powerset = filterM (\_ -> [True, False])