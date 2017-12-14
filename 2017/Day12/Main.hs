module Main
  ( main
  ) where

import Control.Lens (view, _1)
import Data.Char (isDigit)
import Data.Graph
import Data.Maybe (listToMaybe)
import Text.ParserCombinators.ReadP

idP :: ReadP Int
idP = read <$> many1 (satisfy isDigit)

connectionsP :: ReadP [Int]
connectionsP = sepBy idP (string ", ")

rowP :: ReadP (Int, [Int])
rowP = (,) <$> idP <* string " <-> " <*> connectionsP <* eof

parseRow :: String -> Maybe (Int, [Int])
parseRow = listToMaybe . map fst . readP_to_S rowP

buildGraph :: [(Int, [Int])] -> Graph
buildGraph =
  view _1 .
  graphFromEdges .
  map (\(program, programs) -> ((), program, programs))

reachableFromZero :: [(Int, [Int])] -> [Int]
reachableFromZero = flip reachable 0 . buildGraph

groups :: [(Int, [Int])] -> Forest Int
groups = components . buildGraph

main :: IO ()
main = do
  input <- (mapM parseRow . lines) <$> getContents
  print ((length . reachableFromZero) <$> input)
  print ((length . groups) <$> input)
