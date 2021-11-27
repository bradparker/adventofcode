{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Criterion.Main (bench, bgroup, defaultMain, whnf)
import Data.Maybe (listToMaybe)
import SubsetSum (sumToTarget)

parseInput :: String -> [Int]
parseInput = map (read @Int) . words

part1 :: [Int] -> Maybe Int
part1 = fmap product . listToMaybe . sumToTarget 2 2020

part2 :: [Int] -> Maybe Int
part2 = fmap product . listToMaybe . sumToTarget 3 2020

main :: IO ()
main = do
  nums <- parseInput <$> getContents
  print $ part1 nums
  print $ part2 nums

  defaultMain
    [ bgroup
        "Part 1"
        [ bench "Solve" (whnf part1 nums)
        ],
      bgroup
        "Part 2"
        [ bench "Solve" (whnf part2 nums)
        ]
    ]
