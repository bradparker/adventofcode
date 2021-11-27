{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Applicative (some)
import Control.Arrow ((&&&))
import Data.Bits (xor)
import Data.Char (isAlpha, isDigit)
import Data.Function ((&))
import Data.List (elemIndices)
import Data.Maybe (listToMaybe)
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    eof,
    readP_to_S,
    satisfy,
    string,
  )

type Parser a = ReadP a

type Range = (Int, Int)

type Rule = (Range, Char)

type Entry = (Rule, String)

intP :: Parser Int
intP = read <$> some (satisfy isDigit)

rangeP :: Parser Range
rangeP = (,) <$> intP <* char '-' <*> intP

ruleP :: Parser Rule
ruleP = (,) <$> rangeP <* char ' ' <*> satisfy isAlpha

entryP :: Parser Entry
entryP = (,) <$> ruleP <* string ": " <*> some (satisfy isAlpha)

entriesP :: Parser [Entry]
entriesP = some (entryP <* char '\n') <* eof

parseInput :: String -> Maybe [Entry]
parseInput = fmap fst . listToMaybe . readP_to_S entriesP

isWithin :: Range -> Int -> Bool
isWithin (lo, hi) n = lo <= n && n <= hi

occurances :: Char -> String -> Int
occurances c = length . filter (== c)

isValid :: Entry -> Bool
isValid ((range, c), password) =
  password
    & occurances c
    & isWithin range

isValid' :: Entry -> Bool
isValid' (((posA, posB), c), password) =
  password
    & elemIndices c
    & map (+ 1)
    & (elem posA &&& elem posB)
    & uncurry xor

main :: IO ()
main = do
  Just entries <- parseInput <$> getContents
  print $ length . filter isValid $ entries
  print $ length . filter isValid' $ entries
