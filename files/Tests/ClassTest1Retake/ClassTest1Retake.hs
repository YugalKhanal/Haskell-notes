-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module ClassTest1Retake (checkPeriodic, divisibleByIndex, findCubes, edit, edits, solvable) where

import Data.List
import Data.Char

import Types

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

{- Question 1 -}
checkPeriodic :: String -> Int -> Bool
checkPeriodic [] _ = True
checkPeriodic s n = all (==True) [s !! i == s !! (i + n) | i <- [0..length s - 1], i + n < length s]

{- Question 2 -}
divisibleByIndex :: [Int] -> [Bool]
divisibleByIndex [] = []
divisibleByIndex xs = map (\(x,i) -> x `mod` i == 0) (zip xs [1..])


{- Question 3 -}
findCubes :: Int -> [(Int,Int,Int)]
findCubes n = [(x,y,z) | x <- [1..n], y <- [x..n], z <- [y..n], x^3 + y^3 + z^3 == n]

{- Question 4 -}
edit :: EditCommand -> Text -> Text
edit MoveLeft (l, r)
    | null l = (l, r)  --left string is empty, return the text unchanged
    | otherwise = (tail l, head l : r)  -- move cursor to the left
edit (Insert c) (l, r) = (c : l, r)  -- insert a character at the cursor
edit MoveRight (l, r) = (head r : l, tail r)  -- move cursor to the right
edit BackSpace (l, r) = (tail l, r)  -- delete character to the left of cursor

edits :: [EditCommand] -> Text -> Text
edits [] text = text
edits (command:commands) text = edits commands (edit command text)  -- recursively apply edit commands to the text

        
{- Question 5 -}
solvable :: ([Bool] -> Bool) -> Int -> Bool
solvable f n = f [x `mod` i == 0 | x <- [1..n], i <- [1..n]]
