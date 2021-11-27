module Main
  ( main,
  )
where

import Control.Lens (view, _1)
import Data.List (find)
import Data.Map (Map, insert, lookup, singleton)
import Data.Maybe (mapMaybe)
import Prelude hiding (lookup)

type Point = (Int, Int)

parseInt :: String -> Int
parseInt = read

right :: Num a => (a, b) -> (a, b)
right (x, y) = (x + 1, y)

up :: Num b => (a, b) -> (a, b)
up (x, y) = (x, y + 1)

left :: Num a => (a, b) -> (a, b)
left (x, y) = (x - 1, y)

down :: Num b => (a, b) -> (a, b)
down (x, y) = (x, y - 1)

block :: Num a => Int -> [(a, a) -> (a, a)]
block k =
  concat
    [ replicate (2 * k + 1) right,
      replicate (2 * k + 1) up,
      replicate (2 * k + 2) left,
      replicate (2 * k + 2) down
    ]

spiral :: [Point]
spiral = scanl (flip ($)) (0, 0) (foldMap block [0 ..])

taxiDistance :: Point -> Point -> Int
taxiDistance (a, b) (c, d) = abs (a - c) + abs (b - d)

distance :: Int -> Int
distance n = taxiDistance (0, 0) (spiral !! (n - 1))

neighbours :: Point -> [Point]
neighbours p =
  map
    ($ p)
    [ right,
      up . right,
      up,
      up . left,
      left,
      down . left,
      down,
      down . right
    ]

pointSum :: Point -> Map Point Int -> Int
pointSum point sums =
  sum $ mapMaybe (`lookup` sums) (neighbours point)

stepSum ::
  (Int, Map Point Int) -> Point -> (Int, Map Point Int)
stepSum (_, sums) point = (sum', insert point sum' sums)
  where
    sum' = pointSum point sums

stepSums :: [(Int, Map Point Int)]
stepSums =
  scanl stepSum (1, singleton (0, 0) 1) (tail spiral)

firstSumAbove :: Int -> Maybe Int
firstSumAbove n =
  view _1
    <$> find ((> n) . view _1) stepSums

-- usage: echo "361527" | cabal run exe:day-03
main :: IO ()
main = do
  n <- parseInt <$> getLine
  putStrLn "Distance from: "
  print $ distance n
  putStrLn "First sum greater than: "
  print $ firstSumAbove n
