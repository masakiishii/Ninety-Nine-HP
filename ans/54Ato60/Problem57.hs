module Problem57 (Tree(..)) where

data Tree a = Empty
     | Branch a (Tree a) (Tree a)
       deriving (Show, Eq)


add :: Ord a => a -> Tree a -> Tree a
add x Empty = Branch x Empty Empty
add x t@(Branch y left right) = case compare x y of
  LT -> Branch y (add x left) right
  GT -> Branch y left (add x right)
  EQ -> t


construct xs = foldl (flip add) Empty xs
