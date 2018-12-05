module Day5 (day05a, day05b) where

import           Data.Char                      ( toLower )
import           Util

filterUnits :: String -> Int
filterUnits = length . foldr step []
  where
    step x (y : xs) | x /= y && toLower x == toLower y = xs
    step x xs = x : xs

minPoly :: String -> Int
minPoly str = minimum $ fmap filterUnits $ redPoly <$> ['a' .. 'z']
    where redPoly c = filter ((c /=) . toLower) str

day05a :: String -> String
day05a = show . filterUnits . strip

day05b :: String -> String
day05b = show . minPoly . strip
