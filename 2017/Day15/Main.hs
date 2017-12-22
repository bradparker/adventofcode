module Main
  ( main
  ) where

import Data.Char (intToDigit)
import Data.Function (on)
import Numeric (showIntAtBase)

divisibleBy :: Int -> Int -> Bool
divisibleBy n = (0 ==) . (`rem` n)

bits :: Int -> String
bits = flip (showIntAtBase 2 intToDigit) ""

generate :: Int -> Int -> Int
generate prev factor = (prev * factor) `rem` 2147483647

generator :: Int -> Int -> [Int]
generator val initial = scanl generate initial (repeat val)

comp :: Int -> Int -> Bool
comp = on (==) (take 16 . reverse . bits)

solve1 :: Int -> Int -> Int
solve1 a b =
  length
    (filter
       (uncurry comp)
       (take 40000000 (zip (generator 16807 a) (generator 48271 b))))

solve2 :: Int -> Int -> Int
solve2 a b =
  length
    (filter
       (uncurry comp)
       (take
          5000000
          (zip
             (filter (divisibleBy 4) (tail (generator 16807 a)))
             (filter (divisibleBy 8) (tail (generator 48271 b))))))

main :: IO ()
main = do
  [a, b] <- (map read . words) <$> getLine
  print $ solve1 a b
  print $ solve2 a b
