module Main
  ( main
  ) where

import Data.Bits (xor)
import Data.Char (ord)
import Data.List.Split (chunksOf, splitOn)
import Numeric (showHex)

parse :: String -> [Int]
parse = (read <$>) . splitOn ","

parse2 :: String -> [Int]
parse2 = map ord

revSection :: Int -> Int -> [a] -> [a]
revSection offset size xs =
  take
    len
    (drop
       (len - pos)
       (cycle
          (reverse (take size (drop pos (cycle xs))) ++
           take
             (len - size)
             (drop ((pos + size) `mod` len) (cycle xs)))))
  where
    len = length xs
    pos = offset `mod` len

tie :: ((Int, Int), [Int]) -> Int -> ((Int, Int), [Int])
tie ((position, skip), xs) size =
  ( (position + size + skip, skip + 1)
  , revSection position size xs)

knots :: [Int] -> ((Int, Int), [Int])
knots = foldl tie ((0, 0), [0 .. 255])

solve :: [Int] -> Int
solve = product . take 2 . snd . knots

leftPad :: Int -> a -> [a] -> [a]
leftPad targetLength pad xs =
  replicate (targetLength - length xs) pad ++ xs

solve2 :: [Int] -> String
solve2 =
  concatMap ((leftPad 2 '0' . (`showHex` "")) . foldr1 xor) .
  chunksOf 16 .
  snd .
  knots . concat . replicate 64 . (++ [17, 31, 73, 47, 23])

main :: IO ()
main = do
  raw <- getLine
  print $ solve (parse raw)
  print $ solve2 (parse2 raw)
