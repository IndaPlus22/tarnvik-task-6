module F1 where

import Data.List  

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib (n-2)

isConsonant x = not ((elem x)"aeyuio")

rovarsprak :: String -> String
rovarsprak [] = []
rovarsprak (x:xs)
    | isConsonant x = x : 'o' : x : rovarsprak xs
    | otherwise = x : rovarsprak xs

--I know its ugly but atleast it works 
karpsravor :: String -> String
karpsravor [] = []
karpsravor (x:y:z:xs)
    | isConsonant x = x : karpsravor xs
    | otherwise = x : karpsravor (y:z:xs)
karpsravor (x:_:xs) = x : karpsravor xs
karpsravor (x:xs) = x : karpsravor xs

medellangd :: String -> Double
medellangd s = 1.0
skyffla s = s