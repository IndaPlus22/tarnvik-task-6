module F1 where

import Data.List  

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib (n-2)

vokaler = "aeyuio"
--vokaler = ['a', 'e', 'y', 'u', 'i', 'o']

rovarsprak :: String -> String
rovarsprak s = concat( map ( addo ) s)

addo :: Char -> String
addo c 
    | not ((elem c) vokaler) = c : 'o' : c : []
    | otherwise            = c : []

--rovarsprak s = concat([c : 'o' : c : []| c <- s, not ((elem c) vokaler)])

karpsravor :: String -> String
karpsravor s = s

removeRovarsprak ch st = [c | c <- st, (ch : 'o' : ch : []) `isInfixOf` st]

isRovarsprak :: Char -> String -> Bool
isRovarsprak c s = (c : 'o' : c : []) `isInfixOf` s

medellangd :: String -> Double
medellangd s = 1.0
skyffla s = s