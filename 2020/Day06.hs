{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Applicative (some, (<|>))
import Data.Set (Set)
import qualified Data.Set as Set
import Parser (Parser, eof, newline, parse, satisfy)

alphabet :: String
alphabet = ['a' .. 'z']

singleAnswerP :: Parser String
singleAnswerP = some (satisfy (`elem` alphabet)) <* newline

groupAnswersP :: Parser [String]
groupAnswersP = some singleAnswerP <* (newline <|> eof)

parseInput :: String -> Maybe [[String]]
parseInput = parse (some groupAnswersP <* eof)

uniqueGroupAnswers :: [String] -> Set Char
uniqueGroupAnswers = foldMap Set.fromList

sharedGroupAnswers :: [String] -> Set Char
sharedGroupAnswers = foldr (Set.intersection . Set.fromList) (Set.fromList alphabet)

main :: IO ()
main = do
  Just groups <- parseInput <$> getContents
  print $ sum (map (Set.size . uniqueGroupAnswers) groups)
  print $ sum (map (Set.size . sharedGroupAnswers) groups)
