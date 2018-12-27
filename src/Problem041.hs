-- 12 Problem 41
-- (**) Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.

-- In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. Very rarely, the primes are both bigger than say 50. Try to find out how many such cases there are in the range 2..3000.

-- Example:

-- * (goldbach-list 9 20)
-- 10 = 3 + 7
-- 12 = 5 + 7
-- 14 = 3 + 11
-- 16 = 3 + 13
-- 18 = 5 + 13
-- 20 = 3 + 17
-- * (goldbach-list 1 2000 50)
-- 992 = 73 + 919
-- 1382 = 61 + 1321
-- 1856 = 67 + 1789
-- 1928 = 61 + 1867
-- Example in Haskell:

-- *Exercises> goldbachList 9 20
-- [(3,7),(5,7),(3,11),(3,13),(5,13),(3,17)]
-- *Exercises> goldbachList' 4 2000 50
-- [(73,919),(61,1321),(67,1789),(61,1867)]

module Problem041 (goldbachList) where

goldbachList n m = map goldbach $ dropWhile (<4) $ filter even [n..m]

goldbachList' n m i = filter (\(x,y) -> x > i && y > i) $ goldbachList n m

goldbach a = head $ filter (\(x, y) -> isPrime x && isPrime y) $ map (\e -> (e, a - e)) [1, 3..a `div` 2]
    where
        factors a = filter (isFactor a) [2..a-1]
        isFactor a b = a `mod` b == 0
        isPrime a = null $ factors a
