module Random (rand) where

import System.IO.Unsafe
import System.Random

rand :: Int -> Int  -> Int 
{-# NOINLINE rand #-}
rand min max = unsafePerformIO(getStdRandom (randomR (min, max)))