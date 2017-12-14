{-# LANGUAGE LambdaCase #-}

module Main
  ( main
  ) where

import Control.Applicative (pure, (<*>), (<|>))
import Data.Char (isDigit)
import Data.Functor.Foldable (ListF (Cons, Nil), para)
import Data.Maybe (listToMaybe, mapMaybe)
import Text.ParserCombinators.ReadP (munch1, readP_to_S)

parseInt :: String -> Maybe Int
parseInt =
  fmap (read . fst) .
  listToMaybe . readP_to_S (munch1 isDigit)

parseInput :: String -> [[Int]]
parseInput = map (mapMaybe parseInt . words) . lines

isMultipleOf :: Integral a => a -> a -> Bool
isMultipleOf n = (0 ==) . (`mod` n)

isFactorOf :: Integral a => a -> a -> Bool
isFactorOf n = (0 ==) . mod n

divisibleBy :: Integral a => a -> a -> Bool
divisibleBy n m = isMultipleOf n m || isFactorOf n m

findDivisibleBy :: Integral a => a -> [a] -> Maybe a
findDivisibleBy n = listToMaybe . filter (divisibleBy n)

findDivisiblePair :: Integral c => [c] -> Maybe (c, c)
findDivisiblePair =
  para $ \case
    Nil -> Nothing
    (Cons n (ns, result)) ->
      ((,) <$> pure n <*> findDivisibleBy n ns) <|> result

orderPair :: Ord a => (a, a) -> (a, a)
orderPair (a, b) = (max a b, min a b)

dividePair :: Integral c => (c, c) -> c
dividePair = uncurry div

divideDivisiblePair :: Integral c => [c] -> Maybe c
divideDivisiblePair =
  ((dividePair . orderPair) <$>) . findDivisiblePair

checksum :: Integral c => [[c]] -> c
checksum = sum . mapMaybe divideDivisiblePair

main :: IO ()
main = do
  input <- parseInput <$> getContents
  print $ checksum input
