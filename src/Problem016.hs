-- 6 Problem 16
-- (**) Drop every N'th element from a list.
--
-- Example:
--
-- * (drop '(a b c d e f g h i k) 3)
-- (A B D E G H K)
-- Example in Haskell:
--
-- *Main> dropEvery "abcdefghik" 3
-- "abdeghk"

module Problem016 (dropEvery) where

dropEvery :: [a] -> Int -> [a]
dropEvery [] _     = []
dropEvery x 1      = []
dropEvery x n
    | n > length x = x
    | otherwise    = let (a, b) = splitAt n x
                     in init a ++ dropEvery b n
