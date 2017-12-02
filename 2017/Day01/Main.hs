module Main
  ( main
  ) where

import Data.Array (assocs, listArray, (!))
import Data.Char (digitToInt)

offsetPairs :: Int -> [a] -> [(a, a)]
offsetPairs o xs = map offsetPair (assocs arr)
  where
    len = length xs
    arr = listArray (0, len - 1) xs
    offsetPair (i, x) = (x, arr ! ((i + o) `mod` len))

sumSameAtOffset :: Int -> [Int] -> Int
sumSameAtOffset offset =
  sum . map fst . filter (uncurry (==)) . offsetPairs offset

-- usage: cat ./input.txt | stack exec day-01

main :: IO ()
main = do
  nums <- map digitToInt <$> getLine
  putStrLn "Offset 1:"
  print $ sumSameAtOffset 1 nums
  putStrLn "Offset half length:"
  print $ sumSameAtOffset (length nums `div` 2) nums
