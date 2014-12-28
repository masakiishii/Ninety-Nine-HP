module Probblem63 (Tree(..)) where

import Data.List

data Tree a = Empty | Branch a (Tree a) (Tree a)
     deriving (Show, Eq)

filled :: Tree a -> [[Bool]]
filled Empty = repeat [False]
filled (Branch _ left right) = [True] : zipWith (++) (filled left) (filled right)

completeBinaryTree :: Int -> Tree Char
completeBinaryTree n = generate_tree 1
  where generate_tree x
          | x > n     = Empty
          | otherwise = Branch 'x' (generate_tree (2 * x) )
                                   (generate_tree (2 * x + 1))


isCompleteBinaryTree :: Tree a -> Bool
isCompleteBinaryTree Empty = True
isCompleteBinaryTree t = and $ last_proper : zipWith (==) lengths powers
  where levels      = takeWhile or $ filled t
        lengths     = map (length . filter id) $ init levels
        powers      = iterate (2*) 1
        last_filled = map head $ group $ last levels
        last_proper = head last_filled && (length last_filled) < 3
