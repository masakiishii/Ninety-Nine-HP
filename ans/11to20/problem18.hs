module Problem18 where

import Data.List

slice :: [a] -> Int -> Int -> [a]
slice xs n m = take (m - n + 1) $ drop (n - 1) xs
