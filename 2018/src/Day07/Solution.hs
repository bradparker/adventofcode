module Day07.Solution where

import Control.Applicative (some)
import Control.Arrow ((&&&))
import Data.Array ((!))
import Data.Char (chr, isAlpha, isUpper, ord)
import qualified Data.Graph as Graph
import Data.Graph (Graph, Vertex)
import qualified Data.List as List
import Data.Maybe (listToMaybe)
import Data.Ord (Down(Down))
import Data.Tree (Tree)
import qualified Data.Tree as Tree
import Text.Megaparsec (Parsec, runParser)
import Text.Megaparsec.Char (newline, satisfy, string)

data Instruction = Instruction
  { parent :: Int
  , identifier :: Int
  } deriving (Show)

type Parser = Parsec String String

identifierParser :: Parser Int
identifierParser = ord <$> satisfy ((&&) <$> isAlpha <*> isUpper)

instructionParser :: Parser Instruction
instructionParser =
  Instruction
    <$> (string "Step " *> identifierParser)
    <*> (string " must be finished before step " *> identifierParser <* string " can begin.")

instructionsParser :: Parser [Instruction]
instructionsParser = some (instructionParser <* newline)

onParsedInput :: Show b => Parser a -> (a -> b) -> String -> String
onParsedInput parser act = either show (show . act) . runParser parser ""

instructionGraph :: [Instruction] -> Graph
instructionGraph = Graph.buildG (ord 'A', ord 'Z') . map (identifier &&& parent)

roots :: Graph -> [Vertex]
roots graph = filter (not . null . (graph !)) (Graph.topSort graph)

root :: Graph -> Maybe Vertex
root = listToMaybe . roots

toTree :: Graph -> Maybe (Tree Vertex)
toTree graph = Tree.unfoldTree (\v -> (v, graph ! v)) <$> root graph

levels :: Graph -> [[Vertex]]
levels = maybe [] Tree.levels . toTree

uniquelyReachable :: Graph -> [Vertex] -> [Vertex]
uniquelyReachable graph vertices = filter notReachable vertices
  where
    notReachable current =
      not $
        any (\other -> other /= current && Graph.path graph other current) vertices

uniquelyReachableLevels :: Graph -> [[Vertex]]
uniquelyReachableLevels = (map . uniquelyReachable) <*> levels

instructionOrder :: [Instruction] -> [Int]
instructionOrder =
  reverse .
  foldMap (List.nub . List.sortOn Down) .
  uniquelyReachableLevels . instructionGraph
