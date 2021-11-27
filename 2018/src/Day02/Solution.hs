{-# OPTIONS_GHC -Wall #-}

module Day02.Solution
  ( main,
  )
where

import Control.Arrow ((&&&), (***))
import Control.Monad.State (State, evalState, get, put)
import Data.Bool (bool)
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, listToMaybe)
import Data.Monoid (Sum (..))
import Data.Set (Set)
import qualified Data.Set as Set

occuranceCounts :: Ord a => [a] -> Map a Int
occuranceCounts = foldr (\c m -> Map.insertWith (+) c 1 m) Map.empty

countWhen :: Bool -> Sum Int
countWhen = bool (Sum 0) (Sum 1)

counts :: String -> (Sum Int, Sum Int)
counts = (countWhen . elem 2 &&& countWhen . elem 3) . occuranceCounts

checksum :: Foldable t => t String -> Int
checksum = uncurry (*) . (getSum *** getSum) . foldMap counts

partOne :: String -> String
partOne = show . checksum . lines

partOneMain :: IO ()
partOneMain = putStrLn . partOne =<< readFile "src/Day02/input.txt"

select :: [a] -> [(a, [a])]
select [] = []
select (x : xs) = (x, xs) : [(y, x : ys) | (y, ys) <- select xs]

remainders :: [a] -> [[a]]
remainders = map snd . select

firstRepeatRemainder :: Ord a => [[a]] -> Maybe [a]
firstRepeatRemainder =
  listToMaybe . catMaybes . flip evalState Set.empty . traverse remainderSeen
  where
    remainderSeen :: Ord a => [a] -> State (Set [a]) (Maybe [a])
    remainderSeen as = do
      seen <- get
      let rs = remainders as
      put $ seen <> Set.fromList rs
      pure $ find (`Set.member` seen) rs

partTwo :: String -> String
partTwo = show . firstRepeatRemainder . lines

partTwoMain :: IO ()
partTwoMain = putStrLn . partTwo =<< readFile "src/Day02/input.txt"

main :: IO ()
main = do
  putStrLn "Part One:"
  partOneMain
  putStrLn "Part Two:"
  partTwoMain
