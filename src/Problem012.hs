-- 2 Problem 12
-- (**) Decode a run-length encoded list.
--
-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.
--
-- Example in Haskell:
--
-- P12> decodeModified
--        [Multiple 4 'a',Single 'b',Multiple 2 'c',
--         Multiple 2 'a',Single 'd',Multiple 4 'e']
-- "aaaabccaadeeee"

module Problem012 (decodeModified) where

data ListItem a = Multiple Int a | Single a

decodeModified :: Eq a => [ListItem a] -> [a]
decodeModified []         = []
decodeModified (x:xs)     = helper x ++ decodeModified xs
    where
        helper (Multiple n a) = replicate n a
        helper (Single a)     = [a]
