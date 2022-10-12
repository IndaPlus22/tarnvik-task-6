module F1 where

import Data.List  
import Data.Char

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n 
    | n <= 42 = fibList!!(fromIntegral n)
    | otherwise = fib(n-1) + fib (n-2)

fibList = [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610,987,1597,2584,4181,6765,10946,17711,28657,46368,75025,121393,196418,317811,514229,832040,1346269,2178309,3524578,5702887,9227465,14930352,24157817,39088169,63245986,102334155,165580141,267914296]

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
karpsravor (x:y:xs) = x : karpsravor (y:xs)
karpsravor (x:xs) = x : karpsravor xs 

amountOfWords :: String -> Int
amountOfWords [] = 0
amountOfWords (x:[]) 
    | isAlpha (x) = 1 + amountOfWords []
    | otherwise = amountOfWords []
amountOfWords (x:y:xs)
    | not (isAlpha y) && isAlpha (x) = 1 + amountOfWords (y:xs)
    | otherwise = amountOfWords (y:xs)

amountOfLetters :: String -> Int
amountOfLetters [] = 0
amountOfLetters (x:xs)
    | isAlpha x = 1 + amountOfLetters xs
    | otherwise = amountOfLetters xs

medellangd :: String -> Double
medellangd s = fromIntegral(amountOfLetters s) / fromIntegral(amountOfWords s)

almostThere [] = []
almostThere s = faktisktSkyfflande s : almostThere (removeSkyfflade s)

almostAlmostThere s = concat (almostThere s)

--skyffla :: [a] -> [a]
skyffla s = almostAlmostThere s

--removeSkyfflade :: [a] -> [a]
removeSkyfflade [] = []
removeSkyfflade (x:y:xs) = y : removeSkyfflade (xs)
removeSkyfflade (x:xs) = removeSkyfflade (xs)

--faktisktSkyfflande :: [a] -> [a]
faktisktSkyfflande [] = []
faktisktSkyfflande (x:[]) = x : faktisktSkyfflande []
faktisktSkyfflande (x:xs) = x : faktisktSkyfflande (tail xs)