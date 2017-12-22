module Main
  ( main
  ) where

import Data.List.Split (splitOn)
import Crypto.Hash.Knot (encode, knotted)

solve :: [Int] -> Int
solve = product . take 2 . knotted

solve2 :: String -> String
solve2 = encode

main :: IO ()
main = do
  raw <- getLine
  print $ solve (map read (splitOn "," raw))
  print $ solve2 raw
