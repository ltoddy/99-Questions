-- 8 Problem 37
-- (**) Calculate Euler's totient function phi(m) (improved).

-- See problem 34 for the definition of Euler's totient function. If the list of the prime factors of a number m is known in the form of problem 36 then the function phi(m) can be efficiently calculated as follows: Let ((p1 m1) (p2 m2) (p3 m3) ...) be the list of prime factors (and their multiplicities) of a given number m. Then phi(m) can be calculated with the following formula:

-- phi(m) = (p1 - 1) * p1 ** (m1 - 1) *
--          (p2 - 1) * p2 ** (m2 - 1) *
--          (p3 - 1) * p3 ** (m3 - 1) * ...
-- Note that a ** b stands for the b'th power of a.

module Problem037 (totient) where

totient m = product [(p - 1) * p ^ (c - 1) | (p, c) <- prime_factors_mult m]

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
