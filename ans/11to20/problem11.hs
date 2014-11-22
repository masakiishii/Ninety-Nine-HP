module Problem11 where

import Data.List

data ListItem a = Single a | Multiple Int a
     deriving (Show)


encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = [(length x, head x) | x <- group xs]


encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified = map encodeHelper . encode
               where
                 encodeHelper (1, x) = Single x
                 encodeHelper (n, x) = Multiple n x
