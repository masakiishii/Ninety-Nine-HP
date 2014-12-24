module Problem62B (Tree(..)) where

data Tree a = Empty | Branch a (Tree a) (Tree a)
     deriving (Show, Eq)


tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
               (Branch 2 Empty Empty)


levels :: Tree a -> [[a]]
levels Empty = repeat []
levels (Branch a left right) = [a] : zipWith (++) (levels left) (levels right)

atLevel :: Tree a -> Int -> [a]
atLevel t n = levels t !! (n - 1)
