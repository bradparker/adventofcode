module Main
  ( main
  ) where

import Control.Arrow ((***))
import Data.Array (Array, array, assocs, bounds, (!))
import Data.Bifunctor (first, second)
import Data.Foldable (find)
import Data.List (unfoldr)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)

type Point = (Int, Int)

data GridSquare
  = Empty
  | Continue
  | Intersection
  | Letter Char
  deriving (Eq, Show)

type Grid = Array Point GridSquare

gridSquare :: Char -> GridSquare
gridSquare '|' = Continue
gridSquare '-' = Continue
gridSquare ' ' = Empty
gridSquare '+' = Intersection
gridSquare x = Letter x

data Direction
  = North
  | East
  | South
  | West
  deriving (Eq, Show)

move :: Direction -> Point -> Point
move North = second (subtract 1)
move East = first (+ 1)
move South = second (+ 1)
move West = first (subtract 1)

inverse :: Direction -> Direction
inverse North = South
inverse South = North
inverse East = West
inverse West = East

directions :: Point -> Grid -> [Direction]
directions pos g =
  map
    fst
    (filter
       ((/= Empty) . (g !) . uncurry move)
       (zip [North, East, South, West] (repeat pos)))

outside :: (Point, Point) -> Point -> Bool
outside ((lowX, lowY), (highX, highY)) (x, y) =
  not (x >= lowX && y >= lowY && x <= highX && y <= highY)

turn :: Direction -> Point -> Grid -> Direction
turn prevDir pos g =
  fromMaybe
    prevDir
    (listToMaybe
       (filter (/= inverse prevDir) (directions pos g)))

navigate ::
     (Point, Direction, Grid)
  -> Maybe (Maybe Char, (Point, Direction, Grid))
navigate (pos, dir, g)
  | outside (bounds g) pos = Nothing
  | otherwise =
    case g ! pos of
      Continue -> Just (Nothing, (move dir pos, dir, g))
      Intersection ->
        Just
          ( Nothing
          , (move (turn dir pos g) pos, turn dir pos g, g))
      Letter char ->
        Just (Just char, (move dir pos, dir, g))
      Empty -> Nothing

grid :: [String] -> Grid
grid rows =
  array ((0, 0), (width, height)) $ do
    (y, row) <- zip [0 ..] rows
    (x, val) <- zip [0 ..] row
    return ((x, y), gridSquare val)
  where
    width = maximum (map length rows) - 1
    height = length rows - 1

findStart :: Grid -> Maybe Point
findStart =
  (fst <$>) .
  find (uncurry (&&) . (((0 ==) . snd) *** (== Continue))) .
  assocs

collectChars :: Grid -> Maybe String
collectChars g =
  (\start -> catMaybes (unfoldr navigate (start, South, g))) <$>
  findStart g

countSteps :: Grid -> Maybe Int
countSteps g =
  (\start -> length (unfoldr navigate (start, South, g))) <$>
  findStart g

main :: IO ()
main = do
  g <- grid . filter (/= "") . splitOn "\n" <$> getContents
  print $ collectChars g
  print $ countSteps g
