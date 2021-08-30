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
