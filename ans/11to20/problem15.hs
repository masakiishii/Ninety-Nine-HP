module Problem15 where

import Data.List

repli :: [a] -> Int -> [a]
repli xs n = xs >>= replicate n
