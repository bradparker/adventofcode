module Main
  ( main,
  )
where

import Crypto.Hash.Knot (encode, knotted)
import Data.List.Split (splitOn)

solve :: [Int] -> Int
solve = product . take 2 . knotted

solve2 :: String -> String
solve2 = encode

main :: IO ()
main = do
  raw <- getLine
  print $ solve (map read (splitOn "," raw))
  print $ solve2 raw
