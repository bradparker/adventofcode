module Day03.Solution
  ( main
  ) where

import Control.Applicative ((<|>), many, some)
import Data.Either (either)
import Data.Functor (void)
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Megaparsec (Parsec, eof, runParser)
import Text.Megaparsec.Char (char, digitChar, newline, string)

type Parser = Parsec String String

integerP :: Parser Int
integerP = read <$> some digitChar

idP :: Parser Int
idP = char '#' *> integerP

offsetP :: Parser Point
offsetP =
  Point
    <$> integerP <* char ','
    <*> integerP

rectP :: Parser Rect
rectP = do
  i <- idP
  void $ string " @ "
  o <- offsetP
  void $ string ": "
  w <- integerP
  void $ char 'x'
  Rect i o w <$> integerP

rectsP :: Parser [Rect]
rectsP = many (rectP <* (void newline <|> eof))

data Point = Point
  { x :: Int
  , y :: Int
  } deriving (Show, Eq, Ord)

data Rect = Rect
  { identifier :: Int
  , offset :: Point
  , width :: Int
  , height :: Int
  } deriving (Show)

coordinates :: Rect -> Set Point
coordinates rect =
  Set.fromList $
    Point
      <$> [x (offset rect) .. x (offset rect) + width rect - 1]
      <*> [y (offset rect) .. y (offset rect) + height rect - 1]

pointOccurances :: [Set Point] -> Map Point Int
pointOccurances = Map.unionsWith (+) . map (Map.fromSet (const 1))

duplicateArea :: [Rect] -> Int
duplicateArea =
  length .
  Map.filter (> 1) .
  pointOccurances .
  map coordinates

partOne :: String -> String
partOne =
  either show (show . duplicateArea) . runParser rectsP "Advent of Code: Day 03"

partOneMain :: IO ()
partOneMain = putStrLn . partOne =<< readFile "src/Day03/input.txt"

totalOverlap :: [Rect] -> Set Point
totalOverlap =
  Set.fromList .
  Map.keys .
  Map.filter (> 1) .
  pointOccurances .
  map coordinates

findLoner :: [Rect] -> Maybe Rect
findLoner rects = find (\r -> Set.disjoint (coordinates r) overlap) rects
  where
    overlap = totalOverlap rects

partTwo :: String -> String
partTwo =
  either show (show . findLoner) . runParser rectsP "Advent of Code: Day 03"

partTwoMain :: IO ()
partTwoMain = putStrLn . partTwo =<< readFile "src/Day03/input.txt"

main :: IO ()
main = do
  putStrLn "Part one:"
  partOneMain
  putStrLn "Part two:"
  partTwoMain
