module Problem12 where

import Data.List

data ListItem a = Single a | Multiple Int a
     deriving (Show)

decodeModified :: [ListItem a] -> [a]
decodeModified = concatMap decodeHelper
               where
                 decodeHelper (Single x) = [x]
                 decodeHelper (Multiple n x) = replicate n x
