module Main
  ( main,
  )
where

import Data.Bifunctor (bimap, first)
import Data.Bool (bool)
import Data.List (find)

parseInput :: String -> [(Int, Int)]
parseInput =
  map (bimap read (read . drop 2) . span (/= ':')) . lines

scannerPos :: Int -> Int -> Int
scannerPos time range = time `mod` ((range - 1) * 2)

severity :: (Int, Int) -> Int
severity (depth, range) =
  bool 0 (depth * range) (scannerPos depth range == 0)

totalSeverity :: [(Int, Int)] -> Int
totalSeverity = sum . map severity

caught :: Int -> [(Int, Int)] -> Bool
caught delay =
  elem 0 . map (uncurry scannerPos . first (+ delay))

minDelay :: [(Int, Int)] -> Maybe Int
minDelay =
  (fst <$>)
    . find (not . uncurry caught)
    . zip [0 ..]
    . repeat

main :: IO ()
main = do
  input <- parseInput <$> getContents
  print $ totalSeverity input
  print $ minDelay input
