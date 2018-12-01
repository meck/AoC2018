module Day1 (day01a, day01b) where

import           Util
import           Data.Maybe                     ( fromJust )
import           Data.List                      ( scanl' )
import           Data.Set                       ( empty
                                                , insert
                                                , member
                                                , Set
                                                )

day01a :: String -> String
day01a = show . sum . parse

day01b :: String -> String
day01b = show . fromJust . firstDup empty . scanl' (+) 0 . cycle . parse

firstDup :: Ord a => Set a -> [a] -> Maybe a
firstDup _ [] = Nothing
firstDup seen (x : xs) =
    if x `member` seen then Just x else firstDup (x `insert` seen) xs

parse :: String -> [Int]
parse = fmap (read . filter (/= '+')) . linesStrip
