import Data.List

isIn :: (Eq a) => [a] -> [a] -> Bool
isIn xs = any (isInfixOf xs) . tails

isInfixOf' :: (Eq a) => [a] -> [a] -> Bool
isInfixOf'  _     []     = False
isInfixOf' [x]    (y:ys) = x == y
isInfixOf' (x:xs) (y:ys) = (x == y) && isInfixOf' xs ys

tails' :: [a] -> [[a]]
tails' [] = []
tails' all@(x:xs) = all : tails' xs
