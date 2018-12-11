-- 2 Problem 31
-- (**) Determine whether a given integer number is prime.

-- Example:

-- * (is-prime 7)
-- T
-- Example in Haskell:

-- P31> isPrime 7
-- True

module Problem031 (isPrime) where

isPrime :: Integral a => a -> Bool
isPrime n | n < 4 = n /= 1
isPrime n = all ((/=0) . mod n) $ takeWhile (<= m) candidates
        where candidates = (2 : 3 : [x + i | x <- [6, 12..], i <- [-1, 1]])
              m = floor . sqrt $ fromIntegral n
