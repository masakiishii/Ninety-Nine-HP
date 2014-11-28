module Problem23 where

import System.Random
import Control.Monad (replicateM)

rnd_select :: [a] -> Int -> IO [a]
rnd_select [] _ = return []
rnd_select list n
           | n < 0 = error "N must be greater than zero"
           | otherwise = do
               pos <- replicateM n $ getStdRandom $ randomR (0, (length list) - 1)
               return [list !! p | p <- pos]
