module Problem17 where

import Data.List

split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)
