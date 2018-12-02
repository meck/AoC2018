module Day2 (day02a, day02b) where

import           Util
import           Data.List                      ( group
                                                , sort
                                                )
import           Data.Maybe                     ( catMaybes )

day02a :: String -> String
day02a input = show $ numNDiff 2 * numNDiff 3
  where
    numNDiff = length . filter id . (<$> linesStrip input) . hasNDup
    hasNDup n = any ((n ==) . length) . group . sort


day02b :: String -> String
day02b input = filtEq $ headNotEq $ linesStrip input
    where filtEq (a, b) = fmap fst $ filter (uncurry (==)) $ zip a b

headNotEq :: Eq a => [[a]] -> ([a], [a])
headNotEq ys = head $ catMaybes $ go ys
  where
    go (x : xs) = (nNotEq 1 x <$> xs) ++ go xs
    go []       = []
    nNotEq n a b | nDiff == n = Just (a, b)
                 | otherwise  = Nothing
        where nDiff = length $ filter (uncurry (/=)) $ zip a b
