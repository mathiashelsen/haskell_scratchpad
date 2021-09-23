import Data.Char

ror :: String -> String
ror (x:xs) = xs ++ [x]

encode :: String -> String -> String
encode _ [] = []
encode key@(k:ks) (m:msg) = (chr ((ord k) + (ord m))) : (encode (ks ++ [k]) msg)

decode :: String -> String -> String
decode _ [] = []
decode key@(k:ks) (m:msg) = (chr ((ord m) - (ord k))) : (decode (ks ++ [k]) msg)
