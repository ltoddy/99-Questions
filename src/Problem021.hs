-- 1 Problem 21
-- Insert an element at a given position into a list.

-- Example:

-- * (insert-at 'alfa '(a b c d) 2)
-- (A ALFA B C D)
-- Example in Haskell:

-- P21> insertAt 'X' "abcd" 2
-- "aXbcd"

module Problem021 (insertAt) where

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs pos = let (ys, zs) = splitAt (pos - 1) xs
                    in ys ++ x:zs
