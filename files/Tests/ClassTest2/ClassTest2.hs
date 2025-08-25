-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module ClassTest2 (stateTrib, runStateTrib, writeLeaves, collapse, mapLeavesWithAddress, toQuadTree, fromQuadTree) where

import Data.List
import Data.Char

import Control.Monad.State
import Control.Monad.Writer

import Types

-- Question 1
stateTrib :: Integer -> State (Integer,Integer,Integer) ()
stateTrib a
    | a == 1 || a == 0 = return ()
    | otherwise = do
        (x,y,z) <- get
        put (x+0+y+z,x,y)
        stateTrib (a-1)


runStateTrib :: Integer -> Integer
runStateTrib n = let ((),(a,b,c)) = runState (stateTrib n) (1,0,0) in a

-- Question 2
writeLeaves :: Bin a b -> Writer [Either a b] ()
writeLeaves (Lf x) = tell [Left x]
writeLeaves (Nd x l r) = do
    writeLeaves l
    tell [Right x]
    writeLeaves r


-- Question 3
collapse :: Bin (Bin a b) b -> Bin a b
collapse (Lf (Lf x)) = Lf x
collapse (Nd x l r) = Nd x (collapse l) (collapse r)
collapse (Lf (Nd x l r)) = Nd x (collapse (Lf l)) $ collapse (Lf r)

-- Question 4
mapLeavesWithAddress :: (a -> Address -> c) -> Bin a b -> Bin c b
mapLeavesWithAddress f (Lf x) = Lf (f x [])


-- Question 5
toQuadTree :: Image -> QuadTree
toQuadTree [[x]] = P x
toQuadTree xs = N (toQuadTree a) (toQuadTree b) (toQuadTree c) (toQuadTree d)
    where 
            a = take (length xs `div` 2) $ map (take (length xs `div` 2)) xs
            b = take (length xs `div` 2) $ map (drop (length xs `div` 2)) xs
            c = drop (length xs `div` 2) $ map (take (length xs `div` 2)) xs
            d = drop (length xs `div` 2) $ map (drop (length xs `div` 2)) xs


fromQuadTree :: QuadTree -> Image
fromQuadTree (P x) = [[x]]
fromQuadTree (N a b c d) = zipWith (++) a' b' ++ zipWith (++) c' d'
    where
        a' = fromQuadTree a
        b' = fromQuadTree b
        c' = fromQuadTree c
        d' = fromQuadTree d

