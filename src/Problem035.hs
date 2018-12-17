-- 6 Problem 35
-- (**) Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order.

-- Example:

-- * (prime-factors 315)
-- (3 3 5 7)
-- Example in Haskell:

-- > primeFactors 315
-- [3, 3, 5, 7]

module Problem035 (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors a = let (f, f1) = factorPairOf a
                     f' = if prime f then [f] else primeFactors f
                     f1' = if prime f1 then [f1] else primeFactors f1
                 in f' ++ f1'
 where
 factorPairOf a = let f = head $ factors a
                  in (f, a `div` f)
 factors a    = filter (isFactor a) [2..a-1]
 isFactor a b = a `mod` b == 0
 prime a      = null $ factors a
