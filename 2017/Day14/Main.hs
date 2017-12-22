module Main
  ( main
  ) where

import Control.Lens (view, _1)
import Crypto.Hash.Knot (encode)
import Data.Bifunctor (first, second)
import Data.Char (digitToInt, intToDigit)
import Data.Foldable (foldMap)
import Data.Graph (components, graphFromEdges)
import Numeric (showIntAtBase)

leftPad :: Int -> a -> [a] -> [a]
leftPad targetLength pad xs =
  replicate (targetLength - length xs) pad ++ xs

bits :: String -> String
bits =
  foldMap
    ((leftPad 4 '0' . flip (showIntAtBase 2 intToDigit) "") .
     digitToInt) .
  encode

grid :: String -> [String]
grid str =
  map
    (bits . (str ++) . ('-' :) . show)
    ([0 .. 127] :: [Int])

used :: [String] -> Int
used = sum . map (length . filter (== '1'))

north :: (a, Int) -> (a, Int)
north = second (+ 1)

south :: (a, Int) -> (a, Int)
south = second (subtract 1)

east :: (Int, a) -> (Int, a)
east = first (+ 1)

west :: (Int, a) -> (Int, a)
west = first (subtract 1)

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours = ([north, south, east, west] <*>) . pure

edges :: [[a]] -> [(a, (Int, Int), [(Int, Int)])]
edges rows = do
  (y, row) <- zip [0 ..] rows
  (x, val) <- zip [0 ..] row
  return (val, (x, y), neighbours (x, y))

regions :: [String] -> Int
regions =
  length .
  components .
  view _1 .
  graphFromEdges . filter (('1' ==) . view _1) . edges

main :: IO ()
main = do
  input <- grid <$> getLine
  print $ used input
  print $ regions input
