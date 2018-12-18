-- 7 Problem 36
-- (**) Determine the prime factors of a given positive integer.

-- Construct a list containing the prime factors and their multiplicity.

-- Example:

-- * (prime-factors-mult 315)
-- ((3 2) (5 1) (7 1))
-- Example in Haskell:

-- *Main> prime_factors_mult 315
-- [(3,2),(5,1),(7,1)]

module Problem036 (prime_factors_mult) where

prime_factors_mult n = map swap $ encode $ primeFactors n
    where swap (x, y) = (y, x)

primeFactors :: Integer -> [Integer]
primeFactors a = let (f, f1) = factorPairOf a
                     f'      = if prime f then [f] else primeFactors f
                     f1'     = if prime f1 then [f1] else primeFactors f1
                 in f' ++ f1'
    where
        factorPairOf a = let f = head $ factors a
                         in (f, a `div` f)
        factors a      = filter (isFactor a) [2..a - 1]
        isFactor a b   = a `mod` b == 0
        prime a        = null $ factors a

encode :: Eq a => [a] -> [(Int, a)]
encode []     = []
encode [x]    = [(1, x)]
encode (x:xs) = ((length . takeWhile (== x)) xs + 1, x) : (encode . dropWhile (==x)) xs
