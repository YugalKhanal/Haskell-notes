-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}
module ClassTest1 (checkParity, substitution, largestPrimeBetween, strongPrimes, 
executeCommands, atmChange) where
import Types
import Data.Char
---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
{- Question 1 -}
checkParity :: String -> Bool
checkParity [] = True
checkParity x 
 |length x `mod` 8 == 0 = even $ length [y| y <- x, y == '1']
 | otherwise = False


{- Question 2 -}
substitution :: String -> String -> String
substitution plaintext key = map (\x -> if isLetter x then (if isUpper x then toUpper else toLower) (key !! charLabel x) else x) plaintext


{- Question 3 -}
largestPrimeBetween :: Int -> Int
largestPrimeBetween n = last[ k | k <- [n..2*n], isPrime k]

nearestPrimeDown :: Int -> Int
nearestPrimeDown n =  last [x | x <- [1..(n-1)], isPrime x]

nearestPrimeUp :: Int -> Int
nearestPrimeUp n = head [x | x <- [(n+1)..], isPrime x]

average :: Int -> Int
average n = (nearestPrimeUp n + nearestPrimeDown n) `div` 2

isStrongPrime :: Int -> Bool
isStrongPrime n = n > average n

strongPrimes :: Int -> [Int]
strongPrimes n = take n [x | x <- [3..], isPrime x && isStrongPrime x]


{- Question 4 -}
executeCommands :: [Command] -> (Int, Int) -> (Int, Int)
executeCommands [] (x, y) = (x, y)
executeCommands ((MoveDown, n):xs) (x, y) = executeCommands xs (x, y-n)
executeCommands ((MoveUp, n):xs) (x, y) = executeCommands xs (x, y+n)
executeCommands ((MoveLeft, n):xs) (x, y) = executeCommands xs (x-n, y)
executeCommands ((MoveRight, n):xs) (x, y) = executeCommands xs (x+n, y)

{- Question 5 -}
atmChange :: Int -> [Int] -> [(Int, Int)]
atmChange 0 _ = []
atmChange _ [] = []
atmChange n (x:xs) = [(last xs, n `div` last xs)] ++  atmChange (n - (last xs * n `div` last xs)) xs
