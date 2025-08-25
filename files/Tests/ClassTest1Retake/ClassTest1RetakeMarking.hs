module ClassTest1RetakeMarking where

import MarkingCore
import ClassTest1RetakeTestCases
import qualified ClassTest1Retake as Student

import Test.QuickCheck
import Control.Monad

import Types

main :: IO ()
main = runMarking tests True
  where
    tests = [ test_checkPeriodic_true             -- 2 marks
            , test_checkPeriodic_stringlength     -- 2 marks
            , test_checkPeriodic_truncated        -- 2 marks
            , test_checkPeriodic_base             -- 2 marks
            , test_checkPeriodic_reject           -- 2 marks
            , test_divisibleByIndex_first_element -- 2 marks
            , test_divisibleByIndex_true          -- 4 marks
            , test_divisibleByIndex_false         -- 4 marks
            , test_findCubes_3                    -- 2 marks
            , test_findCubes_5                    -- 2 marks
            , test_findCubes_66                   -- 2 marks
            , test_findCubes_134                  -- 2 marks
            , test_findCubes_first_7              -- 2 marks
            , test_edit_MoveRight                 -- 1 mark
            , test_edit_MoveRight_End             -- 1 mark
            , test_edit_MoveLeft                  -- 1 mark
            , test_edit_MoveLeft_Start            -- 1 mark
            , test_edit_Insert                    -- 2 marks
            , test_edit_BackSpace                 -- 1 mark
            , test_edit_BackSpace_Start           -- 1 mark
            , test_edits_correct                  -- 2 marks
            , test_satFun                         -- 2 mark
            , test_unsatFun                       -- 2 marks
            , test_twoSided                       -- 6 marks
            ]

-------------------------------------------------------------------------------
-- Tests: Question 1: Checking Periodicity
-------------------------------------------------------------------------------

test_checkPeriodic_true = Test
  { mark        = 2
  , description = newSection ++
                  "Checking that 'checkPeriodic' returns 'True' for arbitrary\
                  \ string and integer pairs which are periodic with respect\
                  \ to the given integer..."
  , successMsg  = "You got 2 marks because your 'checkPeriodic' returns 'True'\
                  \ for strings which are periodic with respect to the given\
                  \ integer."
  , failMsg     = "Your 'checkPeriodic' function does not return 'True' for\
                  \ strings which are periodic with respect to the given\
                  \ integer."
  , prop        = makeUnaryPropWith
                    prop_checkPeriodic_true
                    arbitraryPeriodicString
                    (const [])
  , condition   = Always
  }

test_checkPeriodic_stringlength = Test
  { mark        = 1
  , description = "Checking that 'checkPeriodic' returns 'True' for strings\
                  \ which are periodic with respect to the length of the\
                  \ string (e.g. 'abcd' is periodic with period 4)..."
  , successMsg  = "You got 1 mark because your 'checkPeriodic' returns 'True'\
                  \ for arbitrary strings with respect to the length of each\
                  \ given string."
  , failMsg     = "Your 'checkPeriodic' function does not return 'True' for\
                  \ arbitrary strings with respect to the length of each given\
                  \ string."
  , prop        = makeUnaryPropWith
                    prop_checkPeriodic_stringlength
                    arbitraryString
                    (const [])
  , condition   = Always
  }

test_checkPeriodic_truncated = Test
  { mark        = 2
  , description = "Checking that 'checkPeriodic' returns 'True' for strings\
                  \ which are truncated (e.g. 'ababa')..."
  , successMsg  = "You got 2 marks because your 'checkPeriodic' function returns\
                  \ 'True' for strings which are truncated."
  , failMsg     = "Your 'checkPeriodic' function does not return 'True' for\
                  \ strings which are truncated."
  , prop        = makeUnaryPropWith
                    prop_checkPeriodic_truncated
                    arbitraryPeriodicStringTruncated
                    (const [])
  , condition   = Always
  }

test_checkPeriodic_base = Test
  { mark        = 1
  , description = "Checking that 'checkPeriodic' returns 'True' for strings\
                   \ consisting of a repeated character (e.g. 'aaaaa')..."
  , successMsg  = "You got 1 mark because your 'checkPeriodic' returns 'True'\
                   \ for strings which consist of a repeated character."
  , failMsg     = "Your 'checkPeriodic' function does not return 'True' for\
                   \ strings which consist of a repeated character."
  , prop        = makeUnaryPropWith
                    prop_checkPeriodic_base
                    arbitraryPeriodicStringBase
                    (const [])
  , condition   = Always
  }

test_checkPeriodic_reject = Test
  { mark        = 4
  , description = "Checking that 'checkPeriodic' returns 'False' for strings\
                  \ which are not periodic for the given period..."
  , successMsg  = "You got 4 marks because your 'checkPeriodic' returns 'False'\
                   \ for strings which are not periodic for the given period."
  , failMsg     = "Your 'checkPeriod' function does not return 'False' for\
                   \ strings which are not periodic for the given period."
  , prop        = makeUnaryPropWith
                    prop_checkPeriodic_reject
                    arbitraryNotPeriodicString
                    (const [])
  , condition   = Always
  }

-------------------------------------------------------------------------------
-- Tests: Question 2: Divisibility Of Indexes
-------------------------------------------------------------------------------

test_divisibleByIndex_first_element = Test
  { mark        = 2
  , description = newSection
                  ++
                  "Checking if 'divisibleByIndex' gives '[True]' for singleton\
                  \ lists of integers..."
  , successMsg  = "You got 2 marks because your 'divisibleByIndex' returns\
                   \ '[True]' for singleton list of integers."
  , failMsg     = "Your 'divisibleByIndex' does not give '[True]' for singleton\
                   \ lists."
  , prop        = makeUnaryPropWith prop_divisibleByIndex_first_element arbitraryFirstElement (const [])
  , condition   = Always
  }

test_divisibleByIndex_true = Test
  { mark        = 4
  , description = "Checking if 'divisibleByIndex' returns 'True' in every\
                   \ position where the given integer is divisible by the\
                   \ the respective index..."
  , successMsg  = "You got 4 marks because your 'divisibleByIndex' correctly\
                   \ gives 'True' in each position where the integer is\
                   \ divisible by the respective index."
  , failMsg     = "Your 'divisibleByIndex' does not give 'True' in each\
                   \ position where the integer is divisible by the respective\
                   \ index."
  , prop        = makeUnaryPropWith prop_divisibleByIndex_true arbitraryInts (const [])
  , condition   = Always
  }

test_divisibleByIndex_false = Test
  { mark        = 4
  , description = "Checking if your 'divisibleByIndex' returns 'False' in every\
                   \ position where the given integer is not divisible by the\
                   \ respective index..."
  , successMsg  = "You got 4 marks because your 'divisibleByIndex' correctly\
                   \ gives 'False' in each position where the integer is NOT\
                   \ divisible by the respective index."
  , failMsg     = "Your 'divisibleByIndex' does not give 'False' in each\
                   \ position where the integer is NOT divisible by the\
                   \ respective index."
  , prop        = makeUnaryPropWith prop_divisibleByIndex_false arbitraryInts (const [])
  , condition   = Always
  }

-------------------------------------------------------------------------------
-- Tests: Question 3: Cubic Triplets
-------------------------------------------------------------------------------

test_findCubes_3 = Test
  { mark        = 2
  , description = newSection ++ "Checking if your 'findCubes' works correctly on input 3..."
  , successMsg  = "You got 2 marks because your 'findCubes' function worked\
                  \ correctly on 3."
  , failMsg     = "Your 'findCubes' did not work correctly on input 3."
  , prop        = makeNullaryProp prop_findCubes_3
  , condition   = Always
  }

test_findCubes_5 = Test
  { mark        = 2
  , description = "Checking if your 'findCubes' works correctly on input 5..."
  , successMsg  = "You got 2 marks because your 'findCubes' correctly returned\
                   \ the empty list for the input 5."
  , failMsg     = "Your 'findCubes' did not return the empty list on input\
                  \ 5."
  , prop        = makeNullaryProp prop_findCubes_5
  , condition   = Always
  }

test_findCubes_66 = Test
  { mark        = 2
  , description = "Checking if your 'findCubes' works correctly on input 66..."
  , successMsg  = "You got 2 marks because your 'findCubes' worked correctly on\
                  \ 66."
  , failMsg     = "Your 'findCubes' did not work correctly on input 66."
  , prop        = makeNullaryProp' prop_findCubes_66 10000000
  , condition   = Always
  }

test_findCubes_134 = Test
  { mark        = 2
  , description = "Checking if your 'findCubes' works correctly on input 134..."
  , successMsg  = "You got 2 marks because your 'findCubes' worked correctly\
                  \ on 134."
  , failMsg     = "Your 'findCubes' did not work correctly on 134."
  , prop        = makeNullaryProp' prop_findCubes_134 10000000
  , condition   = Always
  }

test_findCubes_first_7 = Test
  { mark        = 2
  , description = "Checking if your 'findCubes' finds the first 7 numbers which\
                   \ are the sum of 3 cubes."
  , successMsg  = "You got 2 marks because your 'findCubes' found the first 7\
                   \ numbers which are the sum of 3 cubes."
  , failMsg     = "Your 'findCubes' did not find the first 7 numbers which are\
                   \ the sum of 3 cubes."
  , prop        = makeNullaryProp' prop_findCubes_first_7 10000000
  , condition   = Always
  }

--------------------------------------------------------------------------------
-- Tests: Question 4: Cursor Implementation
--------------------------------------------------------------------------------

test_edit_MoveRight = Test
  { mark        = 1
  , description = newSection
                  ++
                  "Checking if 'edit MoveRight' works correctly when\
                   \ 'MoveRight' is a valid command..."
  , successMsg  = "You got 1 mark because your 'edit' works correctly for\
                   \ 'MoveRight' when it is possible to move right."
  , failMsg     = "Your edit did not move right when it was possible to do so."
  , prop        = makeUnaryPropWith
                    prop_edit_MoveRight
                    arbitraryRightInhabitedStrings
                    (const [])
  , condition   = Always
  }

test_edit_MoveRight_End = Test
  { mark        = 1
  , description = "Checking if 'edit MoveRight' works correctly when\
                   \ 'MoveRight' is NOT a valid command..."
  , successMsg  = "You got 1 mark because your 'edit' works correctly for\
                   \ 'MoveRight' when it is NOT possible to move right."
  , failMsg     = "Your edit did not work correctly when 'MoveRight' is used\
                   \ when the cursor is at the end of the string."
  , prop        = makeUnaryPropWith prop_edit_MoveRight_End arbitraryRightEmptyString (const [])
  , condition   = Always
  }

test_edit_MoveLeft = Test
  { mark        = 1
  , description = "Checking if 'edit MoveLeft' works correctly when\
                   \ 'MoveLeft' is a valid command..."
  , successMsg  = "You got 1 mark because your 'edit' works correctly for\
                   \ 'MoveLeft' when it is possible to move left."
  , failMsg     = "Your 'edit' function did not move left when it was possible\
                  \ to do so."
  , prop        = makeUnaryPropWith
                    prop_edit_MoveLeft
                    arbitraryLeftInhabitedStrings
                    (const [])
  , condition   = Always
  }

test_edit_MoveLeft_Start = Test
  { mark        = 1
  , description = "Checking if 'edit MoveLeft' works correctly when\
                   \ 'MoveLeft' is NOT a valid command..."
  , successMsg  = "You got 1 mark because your 'edit' works correctly for\
                   \ 'MoveLeft' when it is NOT possible to move left."
  , failMsg     = "Your 'edit' did not work correctly when 'MoveLeft' is used\
                   \ when the cursor is at the end of the string."
  , prop        = makeUnaryPropWith prop_edit_MoveLeft_Start arbitraryLeftEmptyString (const [])
  , condition   = Always
  }

test_edit_Insert = Test
  { mark        = 2
  , description = "Checking if 'edit (Insert c)' works correctly for arbitrary\
                   \ strings and characters..."
  , successMsg  = "You got 2 marks because your 'edit (Insert c)' works\
                   \ correctly for arbitrary strings and characters."
  , failMsg     = "Your 'edit' does not correctly insert characters into\
                   \ strings."
  , prop        = makeBinaryPropWith prop_edit_Insert arbitraryStringsAndChar (const [])
  , condition   = Always
  }

test_edit_BackSpace = Test
  { mark        = 1
  , description = "Checking if 'edit BackSpace' works correctly for arbitrary\
                  \ strings..."
  , successMsg  = "You got 1 mark because your 'edit BackSpace' works\
                  \ correctly for arbitrary strings."
  , failMsg     = "Your 'edit' does not correctly remove characters when\
                  \ there is a valid character to remove."
  , prop        = makeUnaryPropWith
                    prop_edit_BackSpace
                    arbitraryLeftInhabitedStrings
                    (const [])
  , condition   = Always
  }

test_edit_BackSpace_Start = Test
  { mark        = 1
  , description = "Checking if 'edit BackSpace' works correctly when\
                   \ 'BackSpace' is NOT a valid command..."
  , successMsg  = "You got 1 mark because your 'edit' works correctly for\
                   \ 'BackSpace' when there is no character to delete."
  , failMsg     = "Your edit did not work correctly when 'BackSpace' is used\
                   \ but there is no character to delete."
  , prop        = makeUnaryPropWith
                    prop_edit_BackSpace_Start
                    arbitraryLeftEmptyString
                    (const [])
  , condition   = Always
  }

test_edits_correct = Test
  { mark        = 2
  , description = "Checking if sequences of commands are executed correctly on\
                   \ arbitrary strings, (with your implementation of\
                   \ edit)..."
  , successMsg  = "You got 2 marks because your 'edits' function correctly\
                   \ executes sequences of commands."
  , failMsg     = "Your 'edits' did not correctly execute sequences of\
                   \ commands."
  , prop        = makeBinaryPropWith prop_edits_correct arbitraryEdits (const [])
  , condition   = Always
  }

--------------------------------------------------------------------------------
-- Tests: Question 5: SAT Problem
--------------------------------------------------------------------------------

test_satFun :: Test
test_satFun = Test
  { mark         = 2
  , description  = newSection
                   ++
                   "Checking if your 'solvable' function correctly determines\
                   \ that the function '\\[x, y, z] -> (x && y) || (x && z)' is\
                   \ solvable..."
  , successMsg   = "You got 2 marks because your 'solvable' function correctly\
                   \ determined that the function\
                   \ '\\[x, y, z] -> (x && y) || (x && z)' is solvable."
  , failMsg      = "Your 'solvable' function did not correctly determine that\
                   \ the function '\\[x, y, z] -> (x && y) || (x && z)' is\
                   \ solvable."
  , prop         = makeNullaryProp prop_satFun
  , condition    = Always
  }


test_unsatFun :: Test
test_unsatFun = Test
  { mark         = 2
  , description  = "Checking if your 'solvable' function correctly determines\
                   \ that the function '\\[x, y] -> not (x || y) && (x && y)' is\
                   \ not solvable..."
  , successMsg   = "You got 2 marks because your solvable function correctly\
                   \ determined that the function\
                   \ '\\[x, y] -> not (x || y) && (x && y)' is not solvable."
  , failMsg      = "Your 'solvable' function did not correctly determine that\
                   \ the function '\\[x, y] -> not (x || y) && (x && y)' is not\
                   \ solvable."
  , prop         = makeNullaryProp prop_unsatFun
  , condition    = Always
  }


test_twoSided :: Test
test_twoSided = Test
  { mark         = 6
  , description  = "Checking if your 'solvable' function correctly determines\
                   \ that the function '\\[x] -> x || not x' is solvable *and*\
                   \ that its dual '\\[x] -> x && not x' is not..."
  , successMsg   = "You got 6 marks because your 'solvable' function correctly\
                   \ determined that the function '\\[x] -> x || not x' is\
                   \ solvable and that its dual '\\[x] -> x && not x' is not."
  , failMsg      = "Your 'solvable' function either failed to determine that\
                   \ the function '\\[x] -> x || not x' is solvable or that its\
                   \ dual '\\[x] -> x && not x' is not."
  , prop         = makeNullaryProp prop_twoSided
  , condition    = Always
  }