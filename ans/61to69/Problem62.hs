module Problem62 (Tree(..)) where

data Tree a = Empty | Branch a (Tree a) (Tree a)
     deriving (Show, Eq)


tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
               (Branch 2 Empty Empty)


internals :: Tree a -> [a]
internals Empty = []
internals (Branch a Empty Empty) = []
internals (Branch a left right) = a : internals left ++ internals right

