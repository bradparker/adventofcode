module Main
  ( main
  ) where

import Control.Arrow ((>>>))
import Control.Lens (view, _1)
import Data.Map
  (Map (..), empty, insert, lookup, singleton)
import Data.Maybe (catMaybes, listToMaybe)
import Prelude hiding (lookup)

type Point = (Int, Int)

type Sum = Int

type StepSum = (Sum, Map Point Sum)

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
    [ replicate (2 * k + 1) right
    , replicate (2 * k + 1) up
    , replicate (2 * k + 2) left
    , replicate (2 * k + 2) down
    ]

spiral :: [Point]
spiral = scanl (flip ($)) (0, 0) (foldMap block [0 ..])

pointAt :: Int -> Point
pointAt = (!!) spiral

taxiDistance :: Point -> Point -> Int
taxiDistance (a, b) (c, d) = abs (a - c) + abs (b - d)

distance :: Int -> Int
distance n = taxiDistance (0, 0) (pointAt (n - 1))

neighbours :: Point -> [Point]
neighbours p =
  map
    ($ p)
    [ right
    , up . right
    , up
    , up . left
    , left
    , down . left
    , down
    , down . right
    ]

pointSum :: Point -> Map Point Int -> Int
pointSum point sums =
  sum $
  catMaybes $ map (flip lookup sums) (neighbours point)

stepSum :: StepSum -> Point -> StepSum
stepSum (sum, sums) point = (sum', insert point sum' sums)
  where
    sum' = pointSum point sums

stepSums :: [StepSum]
stepSums =
  scanl stepSum (1, singleton (0, 0) 1) (tail spiral)

firstSumAbove :: Int -> Maybe Int
firstSumAbove n =
  view _1 <$>
  (listToMaybe (filter ((> n) . view _1) stepSums))

-- usage: echo "361527" | stack exec day-03
main :: IO ()
main = do
  n <- parseInt <$> getLine
  putStrLn "Distance from: "
  print $ distance n
  putStrLn "First sum greater than: "
  print $ firstSumAbove n
