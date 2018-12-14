-- 5 Problem 34
-- (**) Calculate Euler's totient function phi(m).

-- Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.

-- Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.

-- Example:

-- * (totient-phi 10)
-- 4
-- Example in Haskell:

-- * totient 10
-- 4

module Problem032 (totient) where

totient :: Int -> Int
totient 1 = 1
totient a = length $ filter (coprime a) [1..a - 1]
  where coprime a b = gcd a b == 1
