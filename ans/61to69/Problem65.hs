module Problem65 (Tree(..)) where

data Tree a = Empty | Branch a (Tree a) (Tree a)
     deriving (Show, Eq)

type Pos = (Int, Int)


layout :: Tree a -> Tree (a, Pos)
layout t = layoutAux x1 1 sep1 t
  where d = depth t
        ld = leftdepth t
        x1 = 2^(d-1) - 2^(d-ld) + 1
        sep1 = 2^(d-2)
        layoutAux x y sep Empty = Empty
        layoutAux x y sep (Branch a l r) =
                Branch (a, (x,y))
                        (layoutAux (x-sep) (y+1) (sep `div` 2) l)
                        (layoutAux (x+sep) (y+1) (sep `div` 2) r)

depth :: Tree a -> Int
depth Empty = 0
depth (Branch a l r) = max (depth l) (depth r) + 1

leftdepth :: Tree a -> Int
leftdepth Empty = 0
leftdepth (Branch a l r) = leftdepth l + 1

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
