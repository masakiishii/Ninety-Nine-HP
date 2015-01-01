import Text.Parsec.String
import Text.Parsec hiding (Empty)

data Tree a = Empty | Branch a (Tree a) (Tree a)
     deriving (Eq, Show)

pTree :: Parser (Tree Char)
pTree = do
    pBranch <|> pEmpty

pBranch = do
  a <- letter
  do char '('
     t0 <- pTree
     char ','
     t1 <- pTree
     char ')'
     return $ Branch a t0 t1
   <|> return (Branch a Empty Empty)

pEmpty =
    return Empty

stringToTree str =
    case parse pTree "" str of
        Right t -> t
        Left e  -> error (show e)
