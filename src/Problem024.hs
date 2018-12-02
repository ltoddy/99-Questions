-- 4 Problem 24
-- Lotto: Draw N different random numbers from the set 1..M.

-- Example:

-- * (rnd-select 6 49)
-- (23 1 17 33 21 37)
-- Example in Haskell:

-- Prelude System.Random>diff_select 6 49
-- Prelude System.Random>[23,1,17,33,21,37]

module Problem024 (diff_select) where

import System.Random

diff_select :: Int -> Int -> IO [Int]
diff_select n to = diff_select' n [1..to]

diff_select' 0 _  = return []
diff_select' _ [] = error "too few elements to choose from"
diff_select' n xs = do r <- randomRIO (0,(length xs)-1)
                       let remaining = take r xs ++ drop (r+1) xs
                       rest <- diff_select' (n-1) remaining
                       return ((xs!!r) : rest)
