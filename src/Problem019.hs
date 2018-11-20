-- 9 Problem 19
-- (**) Rotate a list N places to the left.

-- Hint: Use the predefined functions length and (++).

-- Examples:

-- * (rotate '(a b c d e f g h) 3)
-- (D E F G H A B C)

-- * (rotate '(a b c d e f g h) -2)
-- (G H A B C D E F)
-- Examples in Haskell:

-- *Main> rotate ['a','b','c','d','e','f','g','h'] 3
-- "defghabc"

-- *Main> rotate ['a','b','c','d','e','f','g','h'] (-2)
-- "ghabcdef"

module Problem019 (rotate) where

rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs 0 = xs
rotate xs n
    | n > 0 = drop n xs ++ take n xs
    | n < 0 = let len = length xs
              in drop (len + n) xs ++ take (len + n) xs
