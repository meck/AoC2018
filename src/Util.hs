module Util (strip, linesStrip, Data.List.Split.splitOn) where

import           Data.List.Split                ( splitOn )
import qualified Data.Text                     as T

strip :: String -> String
strip = T.unpack . T.strip . T.pack

linesStrip :: String -> [String]
linesStrip = fmap (T.unpack . T.strip) . T.lines . T.strip . T.pack
