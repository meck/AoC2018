module Day8 (day08a, day08b) where

import           Data.Tree
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L
import           Data.Void                      ( Void )
import           Data.Maybe                     ( fromMaybe )
import           Safe                           ( atMay )

type SNode = Tree [Int]

day08a :: String -> String
day08a = show . sum . fmap sum . parseSNode

day08b :: String -> String
day08b = show . sNodeVal . parseSNode

sNodeParser :: Parsec Void String SNode
sNodeParser = do
    nC    <- integer
    nD    <- integer
    cs    <- count nC sNodeParser
    mData <- count nD integer
    return $ Node mData cs
    where integer = L.lexeme space L.decimal

parseSNode :: String -> SNode
parseSNode inp = fromMaybe (error "Bad Parse") (parseMaybe sNodeParser inp)

sNodeVal :: SNode -> Int
sNodeVal (Node mData c) = if null c
    then sum mData
    else sum $ maybe 0 sNodeVal . atMay c . pred <$> mData
