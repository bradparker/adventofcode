{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Arrow ((&&&))
import Criterion.Main (bench, bgroup, defaultMain, whnf)
import Data.List (find, tails)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import SubsetSum (sumToTarget)
import Text.Read (readMaybe)

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = takeWhile ((== n) . length) . map (take n) . tails

withPreamblesOf :: Int -> [a] -> [NonEmpty a]
withPreamblesOf n = mapMaybe (nonEmpty . reverse) . chunksOf (n + 1)

uncons :: NonEmpty a -> (a, [a])
uncons = NonEmpty.head &&& NonEmpty.tail

isValid :: NonEmpty Int -> Bool
isValid =
  not
    . null
    . uncurry (sumToTarget 2)
    . uncons

part1 :: Int -> [Int] -> Maybe Int
part1 preambleSize =
  fmap NonEmpty.head
    . find (not . isValid)
    . withPreamblesOf preambleSize

type Window = (Int, Seq Int)

narrow :: Int -> Window -> Window
narrow target (total, s)
  | total <= target = (total, s)
  | otherwise = narrow target (total - sum (Seq.take 1 s), Seq.drop 1 s)

step :: Int -> Window -> Int -> Window
step target (total, s) n = narrow target (total + n, s |> n)

findSequenceSumTo :: Int -> [Int] -> Maybe (Seq Int)
findSequenceSumTo target =
  fmap snd . find ((== target) . fst) . scanl (step target) (0, Seq.empty)

part2 :: Int -> [Int] -> Maybe Int
part2 target =
  fmap (uncurry (+) . (minimum &&& maximum))
    . findSequenceSumTo target

main :: IO ()
main = do
  Just nums <- traverse (readMaybe @Int) . lines <$> getContents
  let Just invalidNumber = part1 25 nums

  print invalidNumber
  print $ part2 invalidNumber nums

  defaultMain
    [ bgroup
        "Part 1"
        [ bench "1" (whnf (part1 25) nums)
        ],
      bgroup
        "Part 2"
        [ bench "1" (whnf (part2 invalidNumber) nums)
        ]
    ]
