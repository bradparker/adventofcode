module Main
  ( main,
  )
where

import Data.Bits ((.&.))
import Data.Function (on)

divisibleBy :: Int -> Int -> Bool
divisibleBy n = (0 ==) . (`rem` n)

generate :: Int -> Int -> Int
generate prev factor = (prev * factor) `rem` 2147483647

generator :: Int -> Int -> [Int]
generator val initial = scanl generate initial (repeat val)

match :: Int -> Int -> Bool
match = on (==) (.&. 0xffff)

solve1 :: Int -> Int -> Int
solve1 a b =
  length
    ( filter
        (uncurry match)
        (take 40000000 (zip (generator 16807 a) (generator 48271 b)))
    )

solve2 :: Int -> Int -> Int
solve2 a b =
  length
    ( filter
        (uncurry match)
        ( take
            5000000
            ( zip
                (filter (divisibleBy 4) (tail (generator 16807 a)))
                (filter (divisibleBy 8) (tail (generator 48271 b)))
            )
        )
    )

main :: IO ()
main = do
  [a, b] <- map read . words <$> getLine
  print $ solve1 a b
  print $ solve2 a b
