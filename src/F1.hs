module F1 where

import Data.List  
import Data.Char

--Fibonacci 
fib :: Integer -> Integer
fib n = fibList !! (fromIntegral n)

fibList = 0 : 1 : next fibList 
    where next (x : y : xs) = (x+y) : next (y : xs)

--Rövarspråk
isConsonant x = not ((elem x)"aeyuio")

rovarsprak :: String -> String
rovarsprak [] = []
rovarsprak (x:xs)
    | isConsonant x = x : 'o' : x : rovarsprak xs
    | otherwise = x : rovarsprak xs

karpsravor :: String -> String
karpsravor [] = []
karpsravor (x:xs)
    | isConsonant x = x : karpsravor (tail(tail xs))
    | otherwise = x : karpsravor (xs)

--Medellängd
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

--Skyffla
--There has to be a better way but atleast this works 
skyffla :: [a] -> [a]
skyffla [] = []
skyffla s = faktisktSkyfflande s ++ skyffla (removeSkyfflade s)

removeSkyfflade :: [a] -> [a]
removeSkyfflade [] = []
removeSkyfflade (x:y:xs) = y : removeSkyfflade (xs)
removeSkyfflade (x:xs) = removeSkyfflade (xs)

faktisktSkyfflande :: [a] -> [a]
faktisktSkyfflande [] = []
faktisktSkyfflande (x:[]) = x : faktisktSkyfflande []
faktisktSkyfflande (x:xs) = x : faktisktSkyfflande (tail xs)