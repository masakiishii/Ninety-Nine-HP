module Problem8 where

import Data.List

compress::(Eq a) => [a] -> [a]
compress = map head . group
