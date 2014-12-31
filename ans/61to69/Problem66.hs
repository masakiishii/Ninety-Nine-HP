module Problem66 (Tree(..)) where

data Tree a = Empty | Branch a (Tree a) (Tree a)
     deriving (Eq, Show)

type Pos = (Int, Int)

layout :: Tree a -> Tree (a, Pos)
layout t = t'
  where (l, t', r) = layoutAux x1 1 t
        x1 = maximum l + 1
        layoutAux :: Int -> Int -> Tree a -> ([Int], Tree (a, Pos), [Int])
        layoutAux x y Empty = ([], Empty, [])
        layoutAux x y (Branch a l r) = (ll', Branch (a, (x,y)) l' r', rr')
          where (ll, l', lr) = layoutAux (x-sep) (y+1) l
                (rl, r', rr) = layoutAux (x+sep) (y+1) r
                sep = maximum (0:zipWith (+) lr rl) `div` 2 + 1
                ll' = 0 : overlay (map (+sep) ll) (map (subtract sep) rl)
                rr' = 0 : overlay (map (+sep) rr) (map (subtract sep) lr)

overlay :: [a] -> [a] -> [a]
overlay [] ys = ys
overlay xs [] = xs
overlay (x:xs) (y:ys) = x : overlay xs ys

tree65 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'e'
                                        (Branch 'd' Empty Empty)
                                        (Branch 'g' Empty Empty)
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 'q' Empty Empty)
                        )
                        Empty
                )
