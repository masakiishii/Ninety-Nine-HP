module Problem6 where

import Control.Applicative


isPalindrome::(Eq a) => [a] -> Bool
isPalindrome = (==) <*> reverse
