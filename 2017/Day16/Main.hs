module Main
  ( main
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad (void)
import           Data.Char (isAlpha, isDigit)
import           Data.Foldable (find, foldlM, toList)
import           Data.List.Split (splitOn)
import           Data.Maybe (listToMaybe, mapMaybe)
import           Data.Monoid ((<>))
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Text.ParserCombinators.ReadP
  (ReadP, char, eof, munch1, readP_to_S, satisfy)

data Move
  = Spin Int
  | Exchange Int
             Int
  | Partner Char
            Char
  deriving (Show)

spinP :: ReadP Move
spinP =
  (Spin . read) <$> (char 's' *> munch1 isDigit <* eof)

exchangeP :: ReadP Move
exchangeP = do
  void (char 'x')
  a <- read <$> munch1 isDigit
  void (char '/')
  b <- read <$> munch1 isDigit
  eof
  return (Exchange a b)

partnerP :: ReadP Move
partnerP = do
  void (char 'p')
  a <- satisfy isAlpha
  void (char '/')
  b <- satisfy isAlpha
  eof
  return (Partner a b)

moveP :: ReadP Move
moveP = spinP <|> exchangeP <|> partnerP

parseMove :: String -> Maybe Move
parseMove = (fst <$>) . listToMaybe . readP_to_S moveP

parseInput :: String -> [Move]
parseInput = mapMaybe parseMove . splitOn ","

findIndex :: (Eq a) => a -> Seq a -> Maybe Int
findIndex a xs =
  fst <$>
  find
    ((a ==) . snd)
    (Seq.zip (Seq.fromList [0 .. (Seq.length xs)]) xs)

spin :: Int -> Seq a -> Seq a
spin n xs =
  uncurry (flip (<>)) (Seq.splitAt (Seq.length xs - n) xs)

exchange :: Int -> Int -> Seq a -> Seq a
exchange a b xs =
  foldr
    (uncurry Seq.update)
    xs
    [(a, Seq.index xs b), (b, Seq.index xs a)]

partner :: Ord a => a -> a -> Seq a -> Maybe (Seq a)
partner a b xs =
  exchange <$> findIndex a xs <*> findIndex b xs <*> pure xs

performMove :: Move -> Seq Char -> Maybe (Seq Char)
performMove (Spin n) xs = Just (spin n xs)
performMove (Exchange a b) xs = Just (exchange a b xs)
performMove (Partner a b) xs = partner a b xs

dance :: [Move] -> Maybe String
dance =
  (toList <$>) .
  foldlM (flip performMove) (Seq.fromList ['a' .. 'p'])

main :: IO ()
main = do
  input <- parseInput <$> getLine
  print $ dance input
  print $ dance (take 1000000000 (cycle input))
