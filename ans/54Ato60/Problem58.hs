module Problem58 (Tree(..)) where

data Tree a = Empty | Branch a (Tree a) (Tree a)
     deriving (Show, Eq)

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = let (q, r) = (n - 1) `quotRem` 2
                 in [Branch 'x' left right | i <- [q .. q + r],
                     left  <- cbalTree i,
                     right <- cbalTree (n - i - 1)]

symCbalTrees n = if n `mod` 2 == 0 then [] else
                   [ Branch 'x' t (reverseTree t) | t <- cbalTree (n `div` 2)]

reverseTree Empty = Empty
reverseTree (Branch x l r ) = Branch x (reverseTree r) (reverseTree l)
