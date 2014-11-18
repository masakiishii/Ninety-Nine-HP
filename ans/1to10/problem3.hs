module Probelm3 where

elementAt::[a] -> Int -> a
elementAt [] _ = error "error"
elementAt xs 0 = error "error"
elementAt (x:_) 1  = x
elementAt (_:xs) n = elementAt xs (n - 1)
