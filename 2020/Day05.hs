{-# OPTIONS_GHC -Wall #-}

module Main where

import Data.List (find, sort)
import Data.Maybe (fromMaybe, listToMaybe)
import Numeric (readInt)

rowSpace :: Char -> Maybe Int
rowSpace c =
  case c of
    'F' -> Just 0
    'B' -> Just 1
    _ -> Nothing

readRowNumber :: String -> Maybe Int
readRowNumber =
  fmap fst
    . listToMaybe
    . readInt 2 (`elem` ['F', 'B']) (fromMaybe (-1) . rowSpace)
    . take 7

columnSpace :: Char -> Maybe Int
columnSpace c =
  case c of
    'L' -> Just 0
    'R' -> Just 1
    _ -> Nothing

readColumnNumber :: String -> Maybe Int
readColumnNumber =
  fmap fst
    . listToMaybe
    . readInt 2 (`elem` ['L', 'R']) (fromMaybe (-1) . columnSpace)
    . take 3
    . drop 7

type Seat = (Int, Int)

readSeat :: String -> Maybe Seat
readSeat str = (,) <$> readRowNumber str <*> readColumnNumber str

seatID :: Seat -> Int
seatID (r, c) = r * 8 + c

readSeatID :: String -> Maybe Int
readSeatID = fmap seatID . readSeat

findHole :: [Int] -> Maybe Int
findHole =
  fmap ((+ 1) . fst)
    . find ((> 1) . uncurry subtract)
    . (zip <$> id <*> drop 1)
    . sort

main :: IO ()
main = do
  Just seatIds <- traverse readSeatID . lines <$> getContents
  print $ maximum seatIds
  print $ findHole seatIds
