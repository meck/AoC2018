{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Day3 (day03a, day03b) where

import           Util
import           Data.List.Split                ( splitWhen )
import           Data.Char                      ( isDigit )
import           Data.Set                       ( Set
                                                , intersection
                                                , union
                                                , empty
                                                , fromList
                                                )

type Point = (Int,Int)

type Box = (Point, Point)

data Claim = Claim { cId :: Int , cBox :: Box }

boxPts :: Box -> Set Point
boxPts ((xO, yO), (xM, yM)) =
    fromList [ (x, y) | x <- [xO .. xM], y <- [yO .. yM] ]

overlapPts :: [Claim] -> Set Point
overlapPts = snd . foldr go (empty, empty)
  where
    go (Claim _ b) (seen, over) =
        (boxPts b `union` seen, union over $ boxPts b `intersection` seen)

noOverlap :: [Claim] -> [Claim]
noOverlap cs =
    filter (null . (`intersection` overlapPts cs) . boxPts . cBox) cs

parse :: String -> Claim
parse str = Claim idc ((x, y), (pred x + w, pred y + h))
  where
    idc : x : y : w : h : _ =
        fmap read $ filter (not . null) $ splitWhen (not . isDigit) str

day03a :: String -> String
day03a = show . length . overlapPts . fmap parse . linesStrip

day03b :: String -> String
day03b = show . cId . head . noOverlap . fmap parse . linesStrip
