module Problem5 where

myReverse::[a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]


myReverse' :: [a] -> [a]
myReverse' list = reverse' list []
  where
    reverse' [] reversed     = reversed
    reverse' (x:xs) reversed = reverse' xs (x:reversed)
