{-# LANGUAGE ParallelListComp, StandaloneDeriving #-}

module ClassTest1RetakeTestCases where

import MarkingCore

import TestCasesUtils
import qualified ClassTest1Retake as Student
import qualified ClassTest1RetakeSolutions as Solutions

import Test.QuickCheck

import Data.Int
import Data.List
import Control.DeepSeq

import Types

-------------------------------------------------------------------------------
-- Question 1: Checking Periodicity
-------------------------------------------------------------------------------

-------------
-- Generators
-------------

alphabet :: String
alphabet = ['a'..'z'] ++ ['A'..'Z']

-- Generate arbitrary strings with 1 <= length <= 10
arbitraryString :: Gen String
arbitraryString = do
                  n <- chooseInt (1,10)
                  vectorOf n (elements alphabet)

-- Generate strings of a single character

arbitraryPeriodicStringBase :: Gen (Int, String)
arbitraryPeriodicStringBase = do
                              n <- chooseInt (1,100)
                              m <- chooseInt (1,n)
                              c <- elements alphabet
                              return (m , take n $ repeat c)

-- Generate arbitrary periodic strings
arbitraryPeriodicString :: Gen (Int, String)
arbitraryPeriodicString =
  do n <- chooseInt (1, 6)
     s <- arbitraryString
     return (length s, concat $ replicate n s)

arbitraryPeriodicStringTruncated :: Gen (Int, String)
arbitraryPeriodicStringTruncated = do
                                   s <- arbitraryString
                                   n <- chooseInt (1,6)
                                   k <- chooseInt (0,length s)
                                   let s' = concat $ replicate n s
                                   return (length s , s' ++ take k s )

arbitraryPeriodAndString :: Gen (Int, String)
arbitraryPeriodAndString = do
                           s <- arbitraryString
                           n <- chooseInt (1,length s)
                           return (n,s)

arbitraryNotPeriodicString :: Gen (Int, String)
arbitraryNotPeriodicString = arbitraryPeriodAndString `suchThat` (\(n,s) -> Solutions.checkPeriodic s n == False)

-------------
-- Props
-------------

-- Check if arbitrary periodic strings are declared periodic
prop_checkPeriodic_true :: (Int,String) -> Property
prop_checkPeriodic_true (n, s) = Student.checkPeriodic s n ~= True

-- Check if arbitrary strings are declared periodic on the length of the string
prop_checkPeriodic_stringlength :: String -> Property
prop_checkPeriodic_stringlength s = Student.checkPeriodic s l ~= True
 where
  l = length s

-- Check if arbitrary periodic strings (truncated) are declared periodic
prop_checkPeriodic_truncated :: (Int,String) -> Property
prop_checkPeriodic_truncated (n , s) = Student.checkPeriodic s n ~= True

-- Check if strings of repeated characters are declared periodic
prop_checkPeriodic_base :: (Int,String) -> Property
prop_checkPeriodic_base (n,s) = Student.checkPeriodic s n ~= True

prop_checkPeriodic_reject :: (Int,String) -> Property
prop_checkPeriodic_reject (n,s) = Student.checkPeriodic s n ~= False

-------------------------------------------------------------------------------
-- Question 2: Divisibility Of Indexes
-------------------------------------------------------------------------------

-------------
-- Generators
-------------

arbitraryInt :: Gen Int
arbitraryInt = chooseAny

arbitraryFirstElement :: Gen [Int]
arbitraryFirstElement = do
                        i <- arbitraryInt
                        return [i]

arbitraryInts :: Gen [Int]
arbitraryInts = listOf1 arbitraryInt

-------------
-- Props
-------------

prop_divisibleByIndex_first_element :: [Int] -> Property
prop_divisibleByIndex_first_element is = Student.divisibleByIndex is ~= [True]

bool_divisibleByIndex_true :: [Int] -> Bool
bool_divisibleByIndex_true is = length is == length bs && and [ b | (index,i,b) <- vs
                                                                  , i `mod ` index == 0]
 where
  bs = deepseq (Student.divisibleByIndex is) (Student.divisibleByIndex is)
  vs = zip3 [1..] is bs

prop_divisibleByIndex_true :: [Int] -> Property
prop_divisibleByIndex_true is = counterexample m (bool_divisibleByIndex_true is)
 where
  bs = deepseq (Student.divisibleByIndex is) (Student.divisibleByIndex is)
  m = "You returned " ++ show bs ++ ". Either you returned a list which has a\
       \ different length to the input list, or one of the entries in your list\
       \ was not 'True' when the integer was divisible by the index."

bool_divisibleByIndex_false :: [Int] -> Bool
bool_divisibleByIndex_false is = length is == length bs && and [ not b | (index,i,b) <- vs
                                                                       , i `mod ` index /= 0]
 where
  bs = deepseq (Student.divisibleByIndex is) (Student.divisibleByIndex is)
  vs = zip3 [1..] is bs

prop_divisibleByIndex_false :: [Int] -> Property
prop_divisibleByIndex_false is = counterexample m (bool_divisibleByIndex_false is)
 where
  bs = deepseq (Student.divisibleByIndex is) (Student.divisibleByIndex is)
  m = "You returned " ++ show bs ++ ". Either you returned a list which has a\
       \ different length to the input list, or one of the entries in your list\
       \ was not 'False' when the integer was not divisible by index."

-------------------------------------------------------------------------------
-- Question 3: Cubic Triplets
-------------------------------------------------------------------------------

-- Check if the cubes which add to 3 are given correctly
prop_findCubes_3 :: Property
prop_findCubes_3 = Student.findCubes 3 ~= [(1,1,1)]

-- Check that no solutions are returned for 5.
prop_findCubes_5 :: Property
prop_findCubes_5 = Student.findCubes 5 ~= []

-- Check if the cubes which add up to 66 are given correctly.
prop_findCubes_66 :: Property
prop_findCubes_66 = Student.findCubes 66 ~= [(1,1,4)]

-- Check if the cubes which add up to 134 are given correctly.
prop_findCubes_134 :: Property
prop_findCubes_134 = Student.findCubes 134 ~= [(1,2,5)]

-- Check if the first 7 numbers which are the sum of three cubes
-- are found.
prop_findCubes_first_7 :: Property
prop_findCubes_first_7 = take 7 (filter (\n -> length (Student.findCubes n) > 0) [1..]) ~= [3,10,17,24,29,36,43]

--------------------------------------------------------------------------------
-- Question 4: Cursor Implementation
--------------------------------------------------------------------------------

deriving instance Show EditCommand

-------------
-- Generators
-------------

arbitraryChar :: Gen Char
arbitraryChar = elements (['a'..'z'] ++ ['A'..'Z'] ++ [' ',','])

arbitraryEditString :: Gen String
arbitraryEditString = listOf arbitraryChar

arbitraryInhabitedString :: Gen String
arbitraryInhabitedString = listOf1 arbitraryChar

arbitraryRightEmptyString :: Gen (String,String)
arbitraryRightEmptyString = do 
                            l <- arbitraryEditString
                            return (l,[])

arbitraryLeftEmptyString :: Gen (String,String)
arbitraryLeftEmptyString = do 
                           r <- arbitraryEditString
                           return ([],r)

arbitraryRightInhabitedStrings :: Gen (String,String)
arbitraryRightInhabitedStrings = do
                                 l <- arbitraryEditString
                                 r <- arbitraryInhabitedString
                                 return (l , r)

arbitraryLeftInhabitedStrings :: Gen (String,String)
arbitraryLeftInhabitedStrings = do
                                l <- arbitraryInhabitedString
                                r <- arbitraryEditString
                                return (l , r)

arbitraryStringsAndChar :: Gen ((String,String),Char)
arbitraryStringsAndChar = do
                          l <- arbitraryEditString
                          r <- arbitraryEditString
                          c <- arbitraryChar
                          return ((l,r),c)

arbitraryInsert :: Gen EditCommand
arbitraryInsert = do
                  c <- arbitraryChar
                  return (Insert c)

arbitraryOtherCommands :: Gen EditCommand
arbitraryOtherCommands = elements [BackSpace,MoveLeft,MoveRight]

arbitraryEditCommands :: Gen [EditCommand]
arbitraryEditCommands = listOf1 (oneof [arbitraryInsert,arbitraryOtherCommands])

arbitraryEdits :: Gen ((String,String),[EditCommand])
arbitraryEdits = do
                 l <- arbitraryEditString
                 r <- arbitraryEditString
                 ecs <- arbitraryEditCommands
                 return ((l,r),ecs)


-------------
-- Props
-------------

-- Check if MoveRight commands are correctly executed
prop_edit_MoveRight :: (String,String) -> Property
prop_edit_MoveRight (l , r:rs) = Student.edit MoveRight (l , r:rs) ~= (r:l , rs)

-- Check if MoveRight commands are correctly executed when MoveRight is not valid
-- i.e the strings are unchanged.
prop_edit_MoveRight_End :: (String,String) -> Property
prop_edit_MoveRight_End (s,_) = Student.edit MoveRight (s,[]) ~= (s,[])

-- Check if MoveLeft commands are correctly executed
prop_edit_MoveLeft :: (String,String) -> Property
prop_edit_MoveLeft (l:ls , r) = Student.edit MoveLeft (l:ls , r) ~= (ls , l:r)

-- Check if MoveLeft commands are correctly executed when MoveLeft is not valid
-- i.e the strings are unchanged.
prop_edit_MoveLeft_Start :: (String,String) -> Property
prop_edit_MoveLeft_Start (_,s) = Student.edit MoveLeft ([],s) ~= ([],s)

-- Check if Insert is correct
prop_edit_Insert :: (String,String) -> Char -> Property
prop_edit_Insert (l,r) c = Student.edit (Insert c) (l,r) ~= (c:l,r)

-- Check if BackSpace is correctly executed
prop_edit_BackSpace :: (String,String) -> Property
prop_edit_BackSpace (l:ls , r) = Student.edit BackSpace (l:ls,r) ~= (ls,r)

-- Check if BackSpace is correctly executed when the is no character to delete
-- i.e the strings are unchanged.
prop_edit_BackSpace_Start :: (String,String) -> Property
prop_edit_BackSpace_Start (_,s) = Student.edit BackSpace ([],s) ~= ([],s)

-- Check if sequences of edits are peformed correctly.
prop_edits_correct :: (String,String) -> [EditCommand] -> Property
prop_edits_correct (l,r) cs = Student.edits cs (l,r) ~= foldl (\a b -> Student.edit b a) (l,r) cs

--------------------------------------------------------------------------------
-- Question 5: SAT Problem
--------------------------------------------------------------------------------

lem :: [Bool] -> Bool
lem = \[x] -> x || not x

contradiction :: [Bool] -> Bool
contradiction  = \[x] -> x && not x

satFun :: [Bool] -> Bool
satFun = \[x, y, z] -> (x && y) || (x && z)

implies :: Bool -> Bool -> Bool
implies x y = not x || y

unsatFun :: [Bool] -> Bool
unsatFun = \[x, y] -> not (x || y) && (x && y)

prop_satFun :: Property
prop_satFun = Student.solvable satFun 3 ~= True

prop_unsatFun :: Property
prop_unsatFun = Student.solvable unsatFun 2 ~= False

prop_twoSided :: Property
prop_twoSided = (Student.solvable lem 1 ~= True) .&&. (Student.solvable contradiction 1 ~= False)