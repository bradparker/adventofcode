module Main
  ( main
  ) where

import Crypto.Hash.Knot (encode)
import Data.Char (digitToInt, intToDigit)
import Data.Foldable (foldMap)
import Data.Function (on)
import Data.List (groupBy)
import Data.Set (Set, fromList, intersection, null)
import Numeric (showIntAtBase)
import Prelude hiding (null)

leftPad :: Int -> a -> [a] -> [a]
leftPad targetLength pad xs =
  replicate (targetLength - length xs) pad ++ xs

bits :: String -> String
bits =
  foldMap
    ((leftPad 4 '0' . flip (showIntAtBase 2 intToDigit) "") .
     digitToInt) .
  encode

grid :: String -> [String]
grid str =
  map
    (bits . (str ++) . ('-' :) . show)
    ([0 .. 127] :: [Int])

used :: [String] -> Int
used = sum . map (length . filter (== '1'))

rowRegions :: String -> [Set Int]
rowRegions =
  map (fromList . map fst) .
  filter (('1' ==) . snd . head) .
  groupBy (on (==) snd) . zip [0 ..]

-- | Gives us the number to add to our running total of regions
-- >>> score (fromList [0, 1, 2, 3, 4]) [(fromList [0]), (fromList [2, 3])]
-- -1
-- >>> score (fromList [0, 1]) [(fromList [0]), (fromList [2, 3])]
-- 0
-- >>> score (fromList [1]) [(fromList [0]), (fromList [2, 3])]
-- 1
-- >>> score (fromList [4]) [(fromList [0]), (fromList [2, 3])]
-- 1
score :: Ord a => Set a -> [Set a] -> Int
score region =
  negate .
  subtract 1 .
  length . filter (not . null) . (intersection region <$>)

newRegions :: (Ord a) => [Set a] -> [Set a] -> Int
newRegions regionsA regionsB =
  sum (map (`score` regionsA) regionsB)

regions :: (Ord a) => [[Set a]] -> Int
regions rows = sum (zipWith newRegions ([] : rows) rows)

-- | So this is where I've gone wrong:
-- >>> :{
-- let test =
--       [ "111"
--       , "101"
--       , "111"
--       ]
-- :}
--
-- >>> regions (map rowRegions test)
-- 0
--
-- Should be 1
main :: IO ()
main = do
  input <- grid <$> getLine
  print $ used input
  print $ regions (map rowRegions input)
