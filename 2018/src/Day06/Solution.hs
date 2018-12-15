{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Day06.Solution (main) where

import Control.Applicative (some)
import Control.Arrow ((&&&))
import Data.Bifunctor (second)
import Data.Either (either)
import qualified Data.Foldable as Foldable
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import Text.Megaparsec (Parsec, runParser)
import Text.Megaparsec.Char (digitChar, newline, string)

data Site = Site
  { siteX :: Int
  , siteY :: Int
  } deriving (Eq, Ord, Show)

data Point = Point
  { pointX :: Int
  , pointY :: Int
  } deriving (Eq, Ord, Show)

newtype Plane = Plane
  { getPlane :: [Point]
  } deriving (Show)

plane :: NonEmpty Site -> Plane
plane sites =
  Plane $
    Point
      <$> [ minX sites .. maxX sites ]
      <*> [ minY sites .. maxY sites ]
  where
    minX = Foldable.minimum . (siteX <$>)
    minY = Foldable.minimum . (siteY <$>)
    maxX = Foldable.maximum . (siteX <$>)
    maxY = Foldable.maximum . (siteY <$>)

distance :: Point -> Site -> Int
distance Point {pointX, pointY} Site {siteX, siteY} =
  abs (pointX - siteX) + abs (pointY - siteY)

distances :: Point -> NonEmpty Site -> NonEmpty (Int, Site)
distances point = fmap (distance point &&& id)

closest :: Point -> NonEmpty Site -> Maybe Site
closest p sites =
  case NonEmpty.sort (distances p sites) of
    (dist, site) :| (dist', _) : _ | dist /= dist' -> Just site
    _ -> Nothing

naiveDelaunay :: NonEmpty Site -> [(Site, Point)]
naiveDelaunay sites = mapMaybe tagClosest $ getPlane (plane sites)
  where
    tagClosest :: Point -> Maybe (Site, Point)
    tagClosest p = (,p) <$> closest p sites

cellAreas :: NonEmpty Site -> Map Site Int
cellAreas = Map.fromListWith (+) . map (second (const 1)) . naiveDelaunay

type Parser = Parsec String String

integerP :: Parser Int
integerP = read <$> some digitChar

siteP :: Parser Site
siteP = Site <$> integerP <* string ", " <*> integerP

sitesP :: Parser (NonEmpty Site)
sitesP = NonEmpty.some1 (siteP <* newline)

partOne :: String -> String
partOne =
  either show (show . Foldable.maximum . cellAreas) .
  runParser sitesP "Advent of Code: Day 06"

regionArea :: NonEmpty Site -> Int
regionArea sites = length $ filter closeEnough $ getPlane $ plane sites
  where
    closeEnough :: Point -> Bool
    closeEnough p = sum (fst <$> distances p sites) < 10000

partTwo :: String -> String
partTwo =
  either show (show . regionArea) .
  runParser sitesP "Advent of Code: Day 06"

main :: IO ()
main = do
  input <- readFile "src/Day06/input.txt"
  putStrLn "Part one:"
  putStrLn $ partOne input
  putStrLn "Part two:"
  putStrLn $ partTwo input
