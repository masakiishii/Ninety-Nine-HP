module Problem60 (Tree(..)) where

import Data.Maybe
import Data.List

data Tree a = Empty | Branch a (Tree a) (Tree a)
     deriving (Show, Eq)

hbalTree x = map fst . hbalTree'
         where hbalTree' 0 = [(Empty, 0)]
               hbalTree' 1 = [(Branch x Empty Empty, 1)]
               hbalTree' n =
                 let t = hbalTree' (n-2) ++ hbalTree' (n-1)
                     in [(Branch x lb rb, h)
                        | (lb, lh) <- t, (rb, rh) <- t,
                          let h = 1 + max lh rh, h == n]

hbalTreeNodes _ 0 = [Empty]
hbalTreeNodes x n = concatMap toFilteredTrees [minHeight .. maxHeight]
    where toFilteredTrees  = filter ((n ==) . countNodes) . hbalTree x
          minNodesSeq      = 0:1:zipWith ((+).(1+)) minNodesSeq (tail minNodesSeq)
          minNodes         = (minNodesSeq !!)
          minHeight        = ceiling $ logBase 2 $ fromIntegral (n+1)
          maxHeight        = (fromJust $ findIndex (>n) minNodesSeq) - 1
          countNodes Empty = 0
          countNodes (Branch _ l r) = countNodes l + countNodes r + 1
