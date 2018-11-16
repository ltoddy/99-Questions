-- 5 Problem 15
-- (**) Replicate the elements of a list a given number of times.
--
-- Example:
--
-- * (repli '(a b c) 3)
-- (A A A B B B C C C)
-- Example in Haskell:
--
-- > repli "abc" 3
-- "aaabbbccc"

module Problem015 (repli) where

repli :: [a] -> Int -> [a]
repli [] _     = []
repli x 1      = x
repli (x:xs) n = replicate n x ++ repli xs n
