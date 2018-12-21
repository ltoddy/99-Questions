-- 11 Problem 40
-- (**) Goldbach's conjecture.

-- Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case. It has been numerically confirmed up to very large numbers (much larger than we can go with our Prolog system). Write a predicate to find the two prime numbers that sum up to a given even integer.

-- Example:

-- * (goldbach 28)
-- (5 23)
-- Example in Haskell:

-- *goldbach 28
-- (5, 23)

module Problem040 (goldbach) where

goldbach a = head $ filter (\(x,y) -> isPrime x && isPrime y) $ map (\e -> (e, a - e)) [1, 3..a `div` 2]
    where
        factors a = filter (isFactor a) [2..a - 1]
        isFactor a b = a `mod` b == 0
        isPrime a = null $ factors a
