{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Applicative (some, (<|>))
import Data.Functor (void)
import Data.Graph
  ( Graph,
    Vertex,
    graphFromEdges,
    reachable,
    transposeG,
  )
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tuple (swap)
import Numeric.Natural (Natural)
import Parser
  ( Parser,
    alpha,
    char,
    eof,
    natural,
    newline,
    parse,
    space,
    string,
  )

type Finish = String

type Colour = String

type Bag = (Finish, Colour)

bagP :: Parser Bag
bagP = (,) <$> some alpha <* space <*> some alpha

type Content = (Natural, Bag)

contentP :: Parser Content
contentP = do
  count <- natural
  space
  bag <- bagP
  space
  void (string "bags" <|> string "bag")
  pure (count, bag)

type Rule = (Bag, [Content])

noContentsP :: Parser [Content]
noContentsP = [] <$ string "no other bags."

contentsP :: Parser [Content]
contentsP = some (contentP <* (void (string ", ") <|> void (char '.')))

ruleP :: Parser Rule
ruleP = do
  bag <- bagP
  void (string " bags contain ")
  content <- noContentsP <|> contentsP
  pure (bag, content)

parseInput :: String -> Maybe [Rule]
parseInput = parse (some (ruleP <* (newline <|> eof)) <* eof)

type BagGraphEdge = (Bag, Bag, [Bag])

type BagGraph = (Graph, Vertex -> BagGraphEdge, Bag -> Maybe Vertex)

bagGraph :: [Rule] -> BagGraph
bagGraph = graphFromEdges . map ruleToEdge
  where
    ruleToEdge :: Rule -> (Bag, Bag, [Bag])
    ruleToEdge (outer, inners) = (outer, outer, map snd inners)

edgeToBag :: BagGraphEdge -> Bag
edgeToBag (b, _, _) = b

allWhichCanContain :: Bag -> BagGraph -> Maybe [Bag]
allWhichCanContain bag (graph, nodeFromVertex, vertexFromKey) =
  map (edgeToBag . nodeFromVertex) . reachable (transposeG graph) <$> vertexFromKey bag

type RuleMap = Map Bag (Map Bag Natural)

ruleMap :: [Rule] -> RuleMap
ruleMap = foldMap (\(bag, contents) -> Map.singleton bag (Map.fromList (map swap contents)))

countChildren :: RuleMap -> Bag -> Natural
countChildren rules bag =
  let children = maybe [] Map.assocs (Map.lookup bag rules)
   in 1 + sum (map (\(child, count) -> count * countChildren rules child) children)

main :: IO ()
main = do
  Just rules <- parseInput <$> getContents
  print $ subtract 1 . length <$> allWhichCanContain ("shiny", "gold") (bagGraph rules)
  print $ subtract 1 (countChildren (ruleMap rules) ("shiny", "gold"))
