module Problem26 where

combination :: Int -> [a] -> [[a]]
combination 0 _  = [[]]
combination _ [] = []
combination n (x:xs) = (map (x:) (combination (n-1) xs)) ++ (combination n xs)
