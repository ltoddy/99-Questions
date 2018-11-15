-- 4 Problem 14
--
-- (*) Duplicate the elements of a list.
--
-- Example:
--
-- * (dupli '(a b c c d))
-- (A A B B C C C C D D)
--
-- Example in Haskell:
--
-- > dupli [1, 2, 3]
-- [1,1,2,2,3,3]

module Problem014 (dupli) where

dupli :: [a] -> [a]
dupli = foldr (\ x -> (++) [x, x]) []
