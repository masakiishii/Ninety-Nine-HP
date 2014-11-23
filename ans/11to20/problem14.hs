module Problem14 where

import Control.Monad

myReplicate :: a -> [a]
myReplicate x = [x, x]


dupli :: [a] -> [a]
dupli []     = []
dupli (x:xs) = myReplicate x ++ dupli xs

dupli' xs = xs >>= (\x -> [x, x])
