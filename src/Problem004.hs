-- 4 Problem 4
-- (*) Find the number of elements of a list.
--
-- Example in Haskell:
--
-- Prelude> myLength [123, 456, 789]
-- 3
-- Prelude> myLength "Hello, world!"
-- 13

module Problem004 (myLength) where

myLength :: [a] -> Int
myLength = foldr (\ _ -> (+) 1) 0
