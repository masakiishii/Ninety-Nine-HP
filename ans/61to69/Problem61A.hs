module Problem61A (Tree(..)) where

data Tree a = Empty | Branch a (Tree a) (Tree a)
     deriving (Show, Eq)


tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

leaves :: Tree a -> [a]
leaves Empty     = []
leaves (Branch a Empty Empty) = [a]
leaves (Branch a left right ) = leaves left ++ leaves right
