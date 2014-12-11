module Problem59 (Tree(..)) where

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
