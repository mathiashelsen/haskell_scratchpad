--quicksort :: (Ord a) => [a] -> [a]
--quicksort (x:[]) = x
--quicksort (x:y:[])
--    | x > y = y:x:[]
--    | otherwise = x:y:[]
--quicksort (x:xs) = 

splitlist :: (Ord a) => a -> [a] -> [a]
splitlist x [] = []
splitlist x [y] = [y]
splitlist x (y:ys)
    | y < x = y : splitlist x ys 
    | otherwise = splitlist x ys ++ [y]

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerOrEqual = [y | y <- xs, y <= x]
        larger = [y | y <- xs, y > x]
    in quicksort smallerOrEqual ++ [x] ++ quicksort larger


quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = 
    let smallerOrEqual = filter (<=x) xs
        larger = filter (>x) xs
    in quicksort' smallerOrEqual ++ [x] ++ quicksort' larger
