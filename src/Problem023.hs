-- 3 Problem 23
-- Extract a given number of randomly selected elements from a list.

-- Example:

-- * (rnd-select '(a b c d e f g h) 3)
-- (E D A)
-- Example in Haskell:

-- Prelude System.Random>rnd_select "abcdefgh" 3 >>= putStrLn
-- eda

module Problem023 (rnd_select) where

import System.Random
import Control.Monad (replicateM)

import Control.Monad (replicateM)
rnd_select xs n
    | n < 0     = error "N must be greater than zero."
    | otherwise = replicateM n rand
        where rand = do r <- randomRIO (0, (length xs) - 1)
                        return (xs!!r)
