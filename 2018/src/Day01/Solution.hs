{-# OPTIONS_GHC -Wall #-}

module Day01.Solution where

import Control.Applicative ((<|>), many, some)
import Control.Monad (filterM)
import Control.Monad.State (State, evalState, get, modify)
import Data.Either (either)
import Data.Maybe (listToMaybe)
import Data.Monoid (Endo(..))
import Data.Set as Set
import Data.Set (Set)
import Text.Megaparsec (ParseError, Parsec, runParser)
import Text.Megaparsec.Char (char, digitChar, newline)

type Change = Endo Integer

type Parser = Parsec String String

type ParserError = ParseError String String

integer :: Parser Integer
integer = read <$> some digitChar

increment :: Parser Change
increment = Endo . (+) <$> (char '+' *> integer)

decrement :: Parser Change
decrement = Endo . subtract <$> (char '-' *> integer)

change :: Parser Change
change = increment <|> decrement

changes :: Parser [Change]
changes = many (change <* newline)

applyChanges :: Integer -> [Change] -> Integer
applyChanges n cs = appEndo (mconcat cs) n

partOne :: String -> String
partOne =
  either show (show . applyChanges 0) .
  runParser changes "Advent of Code: Day 01"

partOneMain :: IO ()
partOneMain = putStrLn . partOne =<< readFile "src/Day01/input.txt"

observeChanges :: Integer -> [Change] -> [Integer]
observeChanges = scanl (flip appEndo)

duplicates :: Ord a => [a] -> [a]
duplicates = flip evalState Set.empty . filterM whenSeen
  where
    whenSeen :: Ord a => a -> State (Set a) Bool
    whenSeen a = do
      seen <- get
      modify (Set.insert a)
      pure $ Set.member a seen

firstDuplicate :: Ord a => [a] -> Maybe a
firstDuplicate = listToMaybe . duplicates

partTwo :: String -> String
partTwo =
  either show (show . firstDuplicate . observeChanges 0 . cycle) .
  runParser changes "Advent of Code: Day 01"

partTwoMain :: IO ()
partTwoMain = putStrLn . partTwo =<< readFile "src/Day01/input.txt"

main :: IO ()
main = do
  putStrLn "Part one:"
  partOneMain
  putStrLn "Part two:"
  partTwoMain
