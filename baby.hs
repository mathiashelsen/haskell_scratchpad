doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
    then x
    else 2*x

doubleSmallNumber' x = (if x > 100 then x else 2*x) + 1

boombangs xs = [if x > 10 then "boom" else "bang" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

removeAllNonUpperCase :: [Char] -> [Char]
removeAllNonUpperCase st = [c | c <- st, c `elem` ['A'..'Z']]

factorial :: Integer -> Integer
factorial x = product [1..x]

factorial' x = 
    if x == 0 then 1
    else x*factorial'(x-1)

factorial'' :: Int -> Int
factorial'' 1 = 1
factorial'' x = x*factorial''(x-1)

head' :: [a] -> a
head' [] = error "No no no"
head' (x:_) = x

tail' :: [a] -> [a]
tail' [] = error "No no no"
tail' (_:xs) = xs

firstLetter :: String -> String
firstLetter all@(x:_) = "The first letter of the string " ++ all ++ " is: " ++ show x

hello :: String -> String
hello name
    | name == "John"    = friendlyHello   ++ "John"
    | name == "Fred"    = friendlyHello   ++ "Fred"
    | otherwise         = unfriendlyHello ++ name
    where   friendlyHello = "Hi, nice to see you "
            unfriendlyHello = "Fuck off " 

hello' :: String -> String
hello' "John" = friendlyHello' ++ "John"
hello'  name = unfriendlyHello' ++ name

friendlyHello' :: String
friendlyHello' = "Hi, nice to see you "

unfriendlyHello' :: String
unfriendlyHello' = "Fuck off "

initials :: String -> String -> String
initials firstName lastName = [f] ++ "." ++ [l] ++ "."
    where   (f:_) = firstName
            (l:_) = lastName

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w/h^2, bmi < 25.0]

describeList :: [a] -> String
describeList ls = "The list is " ++ case ls of  []  -> "empty"
                                                [x] -> "a singleton"
                                                xs  -> "a full blown list"

-- Haskell questions 1-10 from the Haskell wiki
last' :: [a] -> a
last' [] = error "Empty list, no last"
last' [x] = x
last' (x:xs) = last' xs

myButLast :: [a] -> a
myButLast xs
    | l == 0 = error "Empty list, no last"
    | l == 1 = head xs
    | l == 2 = x
    | otherwise = myButLast xss
    where   l = length xs
            [x, y] = xs
            xss = tail xs

myButLast' :: [a] -> a
myButLast' [] = error "empty list"
myButLast' (x:[]) = x
myButLast' (x:y:[]) = x
myButLast' (x:xs) = myButLast' xs

myButLast'' :: [a] -> a
myButLast'' [x,_] = x
myButLast'' (_:xs) = myButLast'' xs

nth :: [a] -> Int -> a
nth (x:_) 1 = x
nth (x:xs) n = nth xs (n-1)

myReverse :: [a] -> [a]
myReverse (x:[]) = [x]
myReverse (x:xs) = myReverse(xs) ++ [x]

palindrome :: (Eq a) => [a] -> Bool
palindrome xs = xs == myReverse xs

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f( f x )

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f(x) : myMap f xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p [] = []
myFilter p (x:xs)
    | p x = x : myFilter p xs
    | otherwise = myFilter p xs

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum'' ::(Num a) => [a] -> a  
sum'' = foldl (+) 0

myReverse' :: [a] -> [a]
myReverse' = foldl (flip( : )) []

sumSqrt :: [Float] -> [Float]
sumSqrt xs = scanl (\acc x -> acc + sqrt(x)) 0 xs

-- compress :: (Eq a) => [a] -> [a]
-- compress xs = foldl (\acc x -> if x `elem` acc then acc else x:acc ) [] xs

compress :: (Eq a) => [a] -> [a]
compress (x:xs) = reverse $ foldl(\acc y -> if head acc == y then acc else y:acc) [x] xs

group' :: (Eq a) => [a] -> [[a]]
group' [] = []
group' (x:xs) = (x : takeWhile (==x) xs) : group' (dropWhile (==x) xs)

span' :: (a -> Bool) -> [a] -> ([a], [a])
span' _ [] = ([], [])
span' f xs = (xfirst, xsecond)
    where   xfirst = takeWhile f xs
            xsecond = dropWhile f xs

encode :: (Eq a) => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . group'

glue :: [[a]] -> [a]
glue [] = []
glue (x:xs) = x ++ glue xs

decode :: [(Int, a)] -> [a]
decode xs = glue [ (replicate n x) | (n, x) <- xs]

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f all@(x:xs) = if f x then dropWhile' f xs else all

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : dupli xs

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = (replicate n x) ++ repli xs n

dropEvery :: Int -> [a] -> [a]
dropEvery _ [] = []
dropEvery n xs = (take (n-1) xs) ++ dropEvery n ( drop n xs )

dropEvery' :: Int -> [a] -> [a]
dropEvery' n xs =  map fst $ filter (\(x, i) -> i `mod` n /= 0) $ zip xs [1..]

split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split all@(x:xs) n
    | n > 0 = (x : ys, zs)
    | otherwise = ([], all)
    where (ys, zs) = split xs (n-1)

slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice all@(x:xs) l u
    | l == 1 && u == 0 = []
    | l > 1 = slice xs (l-1) (u-1)
    | u > 0 = x : slice xs l (u-1)

rotate :: [a] -> Int -> [a]
rotate all@(x:xs) n
    | n < 0 = rotate all (n + 1 + length xs)
    | n > 0 = rotate (xs ++ [x]) (n-1)
    | otherwise = all

removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (xs !! (n-1), remList)
    where remList = map fst ( filter (\(x, i) -> i /= n) (zip xs [1..])) 

eratosthenes :: [Int] -> [Int]
eratosthenes (p:xs) = p : (eratosthenes . (filter (\x -> x `mod` p /= 0)) $ xs)

primeList :: Int -> [Int]
primeList n = take n $ eratosthenes [2..]
