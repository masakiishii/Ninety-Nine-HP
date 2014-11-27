module Problem22 where

import Data.List

range :: Int -> Int -> [Int]
range n m
      | n == m = [m]
      | n < m  = n:range (n + 1) m
      | n > m  = n:range (n - 1) m
