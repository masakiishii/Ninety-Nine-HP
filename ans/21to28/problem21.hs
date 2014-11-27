module Problem21 where

import Data.List

insertAt :: [a] -> [a] -> Int -> [a]
insertAt xs ys n = take (n-1) ys ++ xs ++ drop (n-1) ys
