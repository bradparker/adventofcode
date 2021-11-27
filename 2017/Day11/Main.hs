module Main
  ( main,
  )
where

import Data.List.Split (splitOn)

type Point = (Int, Int)

north :: Point -> Point
north (x, y) = (x, y + 1)

northEast :: Point -> Point
northEast (x, y) = (x + 1, y)

southEast :: Point -> Point
southEast (x, y) = (x + 1, y - 1)

south :: Point -> Point
south (x, y) = (x, y - 1)

southWest :: Point -> Point
southWest (x, y) = (x - 1, y)

northWest :: Point -> Point
northWest (x, y) = (x - 1, y + 1)

direction :: String -> Point -> Point
direction "n" = north
direction "ne" = northEast
direction "se" = southEast
direction "s" = south
direction "sw" = southWest
direction "nw" = northWest
direction _ = id

directions :: [String] -> [Point -> Point]
directions = map direction

travel :: [String] -> [Point]
travel = scanl (flip ($)) (0, 0) . directions

distance :: Point -> Point -> Int
distance (x1, y1) (x2, y2) =
  maximum (map abs (zipWith (-) [y2, x2, z2] [y1, x1, z1]))
  where
    z1 = - x1 - y1
    z2 = - x2 - y2

main :: IO ()
main = do
  path <- splitOn "," <$> getLine
  print $ distance (0, 0) (last (travel path))
  print $ maximum (map (distance (0, 0)) (travel path))
