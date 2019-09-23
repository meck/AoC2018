{-# LANGUAGE OverloadedStrings #-}
module Day10 (day10a, day10b) where

import           Data.Bifunctor
import qualified Data.Text as T
import           Control.Monad                  ( foldM )
import Util

type Cord = (Int,Int)
type Velocity = (Int, Int)

newtype Starfield = Starfield [(Cord, Velocity)]

instance Show Starfield where
  show (Starfield starMap) =
    strip $
    unlines $
    fmap
      (\x ->
         case lookup x starMap of
           Just _ -> 'X'
           Nothing -> '.') <$>
    list
    where
      (minCord, maxCord) = bounds $ Starfield starMap
      list =
        [ [(j, i) | j <- [fst minCord .. fst maxCord]]
        | i <- [snd minCord .. snd maxCord]
        ]


bounds :: Starfield -> (Cord, Cord)
bounds (Starfield starMap) = (f minimum, f maximum)
    where f g = bimap' g $ unzip $ fst <$> starMap

size :: Starfield -> Int
size s = abs (xm - x0) * abs (ym - y0)
      where ((x0,y0),(xm,ym)) = bounds s

step :: Starfield -> Starfield
step (Starfield starmap) =
    Starfield $ (\(c, v) -> (uncurry bimap (bimap' (+) v) c, v)) <$> starmap

smallestMap :: [Starfield] -> (Starfield, Int)
smallestMap s = either id id $ foldM f (head s, -1) s
  where
    f (a, i) b | size a < size b = Left (a, i)
               | otherwise       = Right (b, succ i)

parse :: String -> (Cord, Velocity)
parse = bimap' parse' . T.breakOn " velocity" . T.pack
  where
    parse' =
        bimap' (read . T.unpack . T.strip . T.dropWhile (== ','))
            . T.breakOn ", "
            . T.drop 1
            . T.dropEnd 1
            . T.dropWhile (/= '<')

bimap' :: Bifunctor p => (a -> d) -> p a a -> p d d
bimap' f = bimap f f

day10a :: String -> String
day10a =
    show . fst . smallestMap . iterate step . Starfield . fmap parse . lines

day10b :: String -> String
day10b =
    show . snd . smallestMap . iterate step . Starfield . fmap parse . lines
