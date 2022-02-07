module Utils (nextPosition) where

import Objects
import Data.Matrix

-- Return the penultimate or only element of a list, if is empty return the default value
nextPosition :: [a] -> a -> a
nextPosition xs defaultValue = case xs of
  list@(_:y:_) -> list !! (length list - 2)
  [x] -> x
  [] -> defaultValue
