-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE FlexibleContexts #-}

module Homework9 (factorial, applyfuns, updateNodes, eval, run) where

import Types
import Control.Monad.Except
import Control.Monad.State

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------
facHelper :: Integer -> State Integer ()
facHelper 0 = pure ()
facHelper n = if n < 0 then pure () else do
    modify (*n)
    facHelper (n-1)

factorial :: Integer -> Integer
factorial n = snd (runState (facHelper n) 1)



applyfuns :: (a -> c) -> (b -> d) -> Tree a b -> Tree c d
applyfuns g h (Leaf x) = Leaf (h x)
applyfuns g h (Fork l x r) = Fork (applyfuns g h l) (g x) (applyfuns g h r)

str2int :: String -> Int
str2int xs = length xs

int2bool :: Int -> Bool
int2bool n = n /= 0


updateNodes :: Route -> (a -> a) -> BinTree a -> BinTree a
updateNodes xs f Empty = Empty
updateNodes [] f (Node l x r) = Node l (f x) r
-- updateNodes (GoLeft:xs) f (Node l x r) = Node (updateNodes xs f l) x r -- this only applies to the leaf
updateNodes (GoLeft:xs) f (Node l x r) = Node (updateNodes xs f l) (f x) r -- this applies to all nodes you pass through because f(x) is applied to all nodes
-- updateNodes (GoRight:xs) f (Node l x r) = Node l x (updateNodes xs f r)
updateNodes (GoRight:xs) f (Node l x r) = Node l (f x) (updateNodes xs f r)



-- eval (Val i) = pure i
-- eval (Add e f) = do ev <- eval e
--                     fv <- eval f
--                     return (ev + fv)
-- eval (Mult e f) = do ev <- eval e
--                     fv <- eval f
--                      return (ev * fv)
-- eval (Sub e f) = do ev <- eval e
--                     fv <- eval f
--                     return (ev - fv)
-- eval (Div e f) = do ev <- eval e
--                     fv <- eval f
--                     if (fv == 0)
--                       then throwError "Divide by zero!"
--                       else return (ev `div` fv)

eval :: MonadError String m => CalcExpr -> m Int
eval (Val n) = pure n
eval (Add x y) = do
    x' <- eval x
    y' <- eval y
    pure (x' + y')
eval (Mult x y) = do
    x' <- eval x
    y' <- eval y
    pure (x' * y')
eval (Sub x y) = do
    x' <- eval x
    y' <- eval y
    pure (x' - y')
eval (Div x y) = do
    x' <- eval x
    y' <- eval y
    if y' == 0 then throwError "Division by zero" else pure (x' `div` y')
    


run :: (MonadState Int m, MonadError String m) => CalcCmd -> m ()
run EnterC = pure ()
run (StoreC n c) = do
    modify (+n)
    run c
run (AddC n c) = do
    modify (+n)
    run c
run (MultC n c) = do
    modify (*n)
    run c
run (SubC n c) = do
    modify (subtract n)
    run c
run (DivC n c) = do
    if n == 0 then throwError "Division by zero" else modify (`div` n)
    run c

