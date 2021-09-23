import Data.List

isIn :: (Eq a) => [a] -> [a] -> Bool
isIn xs = any (isInfixOf xs) . tails
