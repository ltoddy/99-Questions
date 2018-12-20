-- 10 Problem 39
-- (*) A list of prime numbers.

-- Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.

-- Example in Haskell:

-- P29> primesR 10 20
-- [11,13,17,19]

module Problem039 (primesR) where

primesR :: Integral a => a -> a -> [a]
primesR a b | even a = filter isPrime [a + 1, a + 3 .. b]
            | True   = filter isPrime [a, a + 2 .. b]

isPrime :: Integral a => a -> Bool
isPrime n | n < 4 = n /= 1
isPrime n = all ((/=0) . mod n) $ takeWhile (<= m) candidates
        where candidates = (2 : 3 : [x + i | x <- [6, 12..], i <- [-1, 1]])
              m = floor . sqrt $ fromIntegral n
