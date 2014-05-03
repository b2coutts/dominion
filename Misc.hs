-- miscellaneous utility code that is not specifically related to dominion
module Misc ( shuf ) where

import System.IO
import System.Random
import Text.Printf

-- randomly permutes a list
-- TODO: is this uniform random? Do I care?
shuf :: StdGen -> [a] -> ([a], StdGen)
shuf gen [] = ([], gen)
shuf gen (x:xs) = (take ind rec ++ [x] ++ drop ind rec, gen'')
    where (n, gen') = random gen :: (Int, StdGen)
          ind = mod n $ length xs + 1
          (rec, gen'') = shuf gen' xs
