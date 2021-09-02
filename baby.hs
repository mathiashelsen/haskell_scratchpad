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
