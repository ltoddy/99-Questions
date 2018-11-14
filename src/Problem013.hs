-- 3 Problem 13
-- (**) Run-length encoding of a list (direct solution).
--
-- Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.
--
-- Example:
--
-- * (encode-direct '(a a a a b c c a a d e e e e))
-- ((4 A) B (2 C) (2 A) D (4 E))
-- Example in Haskell:
--
-- P13> encodeDirect "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',
--  Multiple 2 'a',Single 'd',Multiple 4 'e']

module Problem013 (ListItem, encodeDirect) where

data ListItem a = Multiple Int a | Single a
    deriving (Show)

encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect []  = []
encodeDirect [x] = [Single x]
encodeDirect (x:xs) = let len = length (takeWhile (==x) xs)
                          rest = dropWhile (==x) xs
                      in encodeHelper (len + 1) x : encodeDirect rest
    where
        encodeHelper 1 a = Single a
        encodeHelper n a = Multiple n a
