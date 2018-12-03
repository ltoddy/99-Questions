-- 5 Problem 25
-- Generate a random permutation of the elements of a list.

-- Example:

-- * (rnd-permu '(a b c d e f))
-- (B A D C E F)
-- Example in Haskell:

-- Prelude System.Random>rnd_permu "abcdef"
-- Prelude System.Random>"badcef"

module Problem025 (rnd_permu) where

import System.Random (randomRIO)

rnd_permu :: [a] -> IO [a]
rnd_permu []     = return []
rnd_permu (x:xs) = do
    rand <- randomRIO (0, (length xs))
    rest <- rnd_permu xs
    return $ let (ys,zs) = splitAt rand rest
             in ys++(x:zs)

rnd_permu' [] = return []
rnd_permu' xs = do
    rand <- randomRIO (0, (length xs)-1)
    rest <- let (ys,(_:zs)) = splitAt rand xs
            in rnd_permu' $ ys ++ zs
    return $ (xs!!rand):rest
