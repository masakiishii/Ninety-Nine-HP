module Problem13 where

import Data.List

data ListItem a = Single a | Multiple Int a
     deriving (Show)


encode' :: Eq a => [a] -> [(Int, a)]
encode' = foldr helper []
        where
          helper x [] = [(1, x)]
          helper x (y:ys)
                 | x == snd y = (1 + fst y, x):ys
                 | otherwise  = (1, x):y:ys


encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect = map encodeHelper . encode'
             where
               encodeHelper (1, x) = Single x
               encodeHelper (n, x) = Multiple n x
