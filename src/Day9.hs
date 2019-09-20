{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Day9 (day09a, day09b) where

import           Data.List.PointedList.Circular
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M
import           Data.List                      ( foldl' )
import           Data.List.Split                ( splitOn )
import           Data.Char                      ( isDigit )
import           Data.Bifunctor                 ( bimap )

data State = State { scoreCount :: Map Int Int
                   , _board :: PointedList Int }

addMarble :: Int -> PointedList Int -> (Int, PointedList Int)
addMarble m c
    | m `mod` 23 == 0 = (m + _focus newFocus, fromJust $ deleteRight newFocus)
    | otherwise       = (0, insertRight m $ next c)
    where newFocus = moveN (-7) c

playGame :: Int -> Int -> State
playGame players nMarbels = foldl' doMove initState moves
  where
    moves     = zip [1 .. nMarbels] $ cycle [1 .. players]
    initState = State M.empty $ singleton 0
    doMove (State sco brd) (marble, player) = State
        (M.alter (Just . (score +) . fromMaybe 0) player sco)
        board'
        where (score, board') = addMarble marble brd

parse :: String -> (Int, Int)
parse xs = bimap go go (a, b)
  where
    (a : b : _) = splitOn ";" xs
    go          = read . filter isDigit


day09a :: String -> String
day09a =
    show
        . M.foldr (\x acc -> if x > acc then x else acc) 0
        . scoreCount
        . uncurry playGame
        . parse

day09b :: String -> String
day09b =
    show
        . M.foldr (\x acc -> if x > acc then x else acc) 0
        . scoreCount
        . uncurry playGame
        . fmap (* 100)
        . parse
