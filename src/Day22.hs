{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Day22 (day22a, day22b) where

import           Data.List.Split                ( splitOn )
import           Algorithm.Search               ( aStar )
import           Data.Maybe                     ( fromJust )
import           Data.MemoTrie

type Cord = (Int,Int)

type RiskTrie = (Int, Int) :->: (Int,Int)

-- Memoized Trie
riskTrie :: Int -> Cord -> RiskTrie
riskTrie depth target = mMap
  where
    mGeoIndex c = fst $ untrie mMap c
    mErosionLevel c = snd $ untrie mMap c
    mMap = trie (\c -> (geoIndex c, erosionLevel c))
    geoIndex (0, 0)         = 0
    geoIndex c | c == target = 0
    geoIndex (x, 0)         = x * 16807
    geoIndex (0, y)         = y * 48271
    geoIndex (x, y) = product $ mErosionLevel <$> [(x - 1, y), (x, y - 1)]
    erosionLevel c = (mGeoIndex c + depth) `mod` 20183

getRisk :: RiskTrie -> Cord -> Int
getRisk rt p = flip mod 3 $ snd $ untrie rt p

solve1 :: Int -> Cord -> Int
solve1 d t =
    sum
        $   getRisk (riskTrie d t)
        <$> [ (x', y') | x' <- [0 .. fst t], y' <- [0 .. snd t] ]

data Region = Rocky | Wet | Narrow deriving (Eq, Show)

regionFromRisk :: (Eq a, Num a) => a -> Region
regionFromRisk r = case r of
    0 -> Rocky
    1 -> Wet
    2 -> Narrow
    _ -> error "Invalid Risk"

data Gear = Torch | Climbing | Neither deriving (Eq, Show, Ord)

possibleGear :: Region -> [Gear]
possibleGear r = case r of
    Rocky  -> [Climbing, Torch]
    Wet    -> [Climbing, Neither]
    Narrow -> [Torch, Neither]

data State = State { _risktrie :: RiskTrie
                   , _target :: Cord
                   , gear :: Gear
                   , pos :: Cord } deriving (Eq)

instance Ord State where
    compare s s' | pos s /= pos s' = pos s `compare` pos s'
                 | otherwise = gear s `compare` gear s'

possibleMoves :: State -> [State]
possibleMoves (State rt t g c@(x, y)) = mkMoves <> mkGear
  where
    moves =
        filter (elem g . possibleGear . regionFromRisk . getRisk rt) neighbors
    mkMoves = State rt t g <$> moves
    newGear = filter (g /=) $ possibleGear $ regionFromRisk $ getRisk rt c
    mkGear  = (\g' -> State rt t g' c) <$> newGear
    neighbors =
        [ (x', y')
        | (x', y') <- [(x, y + 1), (x - 1, y), (x + 1, y), (x, y - 1)]
        , x' >= 0
        , y' >= 0
        ]

-- This is only called for adjacent states,
-- so it is safe to have this function be partial for non-neighboring states.
costForMove :: State -> State -> Int
costForMove s s' = (pos s `dist` pos s') + (if gear s /= gear s' then 7 else 0)

dist :: Num a => (a, a) -> (a, a) -> a
dist (x1, y1) (x2, y2) = abs (y2 - y1) + abs (x2 - x1)

solve2 :: Int -> Cord -> Maybe (Int, [State])
solve2 d t = aStar possibleMoves costForMove distToEnd isEnd initalState
  where
    initalState = State (riskTrie d t) t Torch (0, 0)
    distToEnd s = pos s `dist` t
    isEnd s = pos s == t && gear s == Torch

parse :: String -> (Int, Cord)
parse st = (read $ drop 7 d, (read dx, read dy))
  where
    d : t : _ = lines st
    [dx, dy]  = splitOn "," $ drop 8 t

day22a :: String -> String
day22a = show . uncurry solve1 . parse

day22b :: String -> String
day22b = show . fst . fromJust . uncurry solve2 . parse
