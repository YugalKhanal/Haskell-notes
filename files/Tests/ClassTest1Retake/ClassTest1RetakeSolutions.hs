-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module ClassTest1RetakeSolutions (checkPeriodic, divisibleByIndex, findCubes, edit, edits, solvable) where

import Data.List
import Data.Char

import Types

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

{- Question 1 -}
checkPeriodic :: String -> Int -> Bool
checkPeriodic xs n | n > length xs = False
checkPeriodic xs n | otherwise = take (length xs) (cycle (take n xs)) == xs

{- Question 2 -}
divisibleByIndex :: [Int] -> [Bool]
divisibleByIndex l = map (\ (k,n) -> k `mod` n == 0) (zip l [1..])

{- Question 3 -}
findCubes :: Int -> [(Int,Int,Int)]
findCubes n = [ (a,b,c) | c <- [1..n]
                        , b <- [1..c]
                        , a <- [1..b]
                        , a^3 + b^3 + c^3 == n ] 

{- Question 4 -}
edit :: EditCommand -> Text -> Text
edit MoveLeft ([],rs) = ([],rs)
edit MoveLeft (l:ls,rs) = (ls,l:rs)
edit MoveRight (ls,[]) = (ls,[])
edit MoveRight (ls,r:rs) = (r:ls,rs)
edit (Insert c) (ls,rs) = (c:ls,rs)
edit BackSpace ([],rs) = ([],rs)
edit BackSpace (l:ls,rs) = (ls,rs)

edits :: [EditCommand] -> Text -> Text
edits [] txt = txt
edits (c:cs) txt = edits cs (edit c txt)
        
{- Question 5 -}
solvable :: ([Bool] -> Bool) -> Int -> Bool
solvable f n = or [f xs | xs <- sequence (replicate n [False,True])]