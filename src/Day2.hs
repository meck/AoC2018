module Day2 (day02a, day02b) where

import           Util
import           Data.List                      ( group
                                                , sort
                                                , intersect
                                                , (\\)
                                                )

day02a :: String -> String
day02a input = show $ numNDiff 2 * numNDiff 3
  where
    numNDiff = length . filter id . (<$> linesStrip input) . hasNDup
    hasNDup n = any ((n ==) . length) . group . sort


day02b :: String -> String
day02b input = head $ go $ linesStrip input
    where go xs =  [x `intersect` x' | x <- xs, x' <- xs, length (x \\ x') == 1]
