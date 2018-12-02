{-# OPTIONS_GHC -Wall #-}

module Day02.Solution
  ( main
  ) where

import Control.Monad.State (State, evalState, get, put)
import Data.Function (on)
import Data.Functor.Const (Const(..))
import Data.Functor.Product (Product(..))
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, listToMaybe)
import Data.Monoid (Sum(..))
import Data.Set (Set)
import qualified Data.Set as Set

occuranceCounts :: Ord a => [a] -> Map a Int
occuranceCounts = foldr (\c m -> Map.insertWith (+) c 1 m) Map.empty

withAnyNOccurance :: Ord a => Int -> [a] -> Bool
withAnyNOccurance n = (n `elem`) . Map.elems . occuranceCounts

type Count = Const (Sum Int)

getCount :: Count a -> Int
getCount = getSum . getConst

count :: Count a
count = Const (Sum 1)

ignore :: Count a
ignore = Const (Sum 0)

countWhenAnyCharOccursNTimes :: Int -> String -> Count a
countWhenAnyCharOccursNTimes n string
  | withAnyNOccurance n string = count
  | otherwise = ignore

runProd :: (f a -> g a -> b) -> Product f g a -> b
runProd f (Pair fa ga) = f fa ga

counts :: Traversable t => t String -> (Int, Int)
counts =
  runProd (on (,) getCount) .
  traverse
    (Pair <$> countWhenAnyCharOccursNTimes 2 <*> countWhenAnyCharOccursNTimes 3)

checksum :: Traversable t => t String -> Int
checksum = uncurry (*) . counts

partOne :: String -> String
partOne = show . checksum . lines

partOneMain :: IO ()
partOneMain = putStrLn . partOne =<< readFile "src/Day02/input.txt"

select :: [a] -> [(a, [a])]
select [] = []
select (x:xs) = (x, xs) : [(y, x : ys) | (y, ys) <- select xs]

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
