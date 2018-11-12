-- 1 Problem 11
-- (*) Modified run-length encoding.
--
-- Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
--
-- Example:
--
-- * (encode-modified '(a a a a b c c a a d e e e e))
-- ((4 A) B (2 C) (2 A) D (4 E))
-- Example in Haskell:
--
-- P11> encodeModified "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',
--  Multiple 2 'a',Single 'd',Multiple 4 'e']

module Problem011 (encodeModified) where

import           Problem010

data ListItem a = Multiple Int a | Single a
    deriving (Show)

encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified = map helper . encode
    where
        helper (1, x) = Single x
        helper (n, x) = Multiple n x
