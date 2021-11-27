{-# LANGUAGE TupleSections #-}

module Main
  ( main,
  )
where

import Control.Applicative (liftA2, (<|>))
import Control.Arrow ((&&&))
import Control.Lens (view, _2)
import Data.Bifunctor (bimap)
import Data.Char (isAlpha, isDigit)
import Data.Graph (Graph, Vertex, graphFromEdges, topSort)
import Data.List (find, partition)
import Data.Map
  ( Map,
    empty,
    fromList,
    insertWith,
    toList,
    (!),
  )
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Tree (Tree, rootLabel, subForest, unfoldTree)
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    eof,
    many1,
    munch1,
    optional,
    readP_to_S,
    string,
  )

nameP :: ReadP String
nameP = munch1 isAlpha

weightP :: ReadP Int
weightP = read <$> (char '(' *> munch1 isDigit <* char ')')

childrenP :: ReadP [String]
childrenP = do
  _ <- string " -> "
  many1 (nameP <* optional (string ", "))

row :: ReadP (String, (Int, [String]))
row = do
  key <- nameP
  _ <- char ' '
  value <- weightP
  keys <- childrenP <|> return []
  eof
  return (key, (value, keys))

parseInput :: String -> Maybe (String, (Int, [String]))
parseInput = listToMaybe . map fst . readP_to_S row

topSortedNodes ::
  ( Graph,
    Vertex -> (node, key, [key]),
    key -> Maybe Vertex
  ) ->
  [(node, key, [key])]
topSortedNodes (graph, getNode, _) =
  map getNode (topSort graph)

findBase :: Ord key => [(key, (value, [key]))] -> Maybe key
findBase =
  (view _2 <$>)
    . listToMaybe
    . topSortedNodes
    . graphFromEdges
    . map (\(k, (v, ks)) -> (v, k, ks))

allEq :: (Eq a) => [a] -> Bool
allEq [] = True
allEq xs = all (== head xs) (tail xs)

occurances ::
  Ord value => [(key, value)] -> [(value, [key])]
occurances =
  toList
    . foldr (\(k, v) acc -> insertWith (++) v [k] acc) empty

singleAndRest ::
  Ord value =>
  [(key, value)] ->
  (Maybe (key, value), [(key, value)])
singleAndRest =
  bimap
    ( listToMaybe
        . ((\(value, keys) -> map (,value) keys) =<<)
    )
    ((\(value, keys) -> map (,value) keys) =<<)
    . partition ((1 ==) . length . snd)
    . occurances

buildTree ::
  (Ord key, Ord value) =>
  [(key, (value, [key]))] ->
  Maybe (Tree value)
buildTree rows = mapToTree (fromList rows) <$> findBase rows

mapToTree ::
  (Ord key, Ord value) =>
  Map key (value, [key]) ->
  key ->
  Tree value
mapToTree m = unfoldTree (\k -> m ! k)

weight :: Num a => Tree a -> a
weight t = rootLabel t + sum (map snd (subTowerWeights t))

subTowerWeights :: Num a => Tree a -> [(a, a)]
subTowerWeights t = map (rootLabel &&& weight) (subForest t)

isBalenced :: (Num value, Eq value) => Tree value -> Bool
isBalenced = allEq . map weight . subForest

findInSubforest ::
  Eq value => value -> Tree value -> Maybe (Tree value)
findInSubforest value =
  find ((value ==) . rootLabel) . subForest

unbalAndBalInSubforest ::
  (Eq value, Ord value, Num value) =>
  Tree value ->
  Maybe (Tree value, Tree value)
unbalAndBalInSubforest t =
  uncurry
    (liftA2 (,))
    ( ((`findInSubforest` t) . fst) =<< unbal,
      listToMaybe
        (mapMaybe ((`findInSubforest` t) . fst) bal)
    )
  where
    (unbal, bal) = (singleAndRest . subTowerWeights) t

unbalDistance :: Num a => (Tree a, Tree a) -> a
unbalDistance (unbal, bal) =
  rootLabel unbal - (weight unbal - weight bal)

unbalencedSubtree :: Tree Int -> Maybe (Tree Int)
unbalencedSubtree =
  find (not . isBalenced) . subForest

valueToCorrect :: Tree Int -> Maybe Int
valueToCorrect t =
  maybe
    (unbalDistance <$> unbalAndBalInSubforest t)
    valueToCorrect
    (unbalencedSubtree t)

main :: IO ()
main = do
  input <- mapMaybe parseInput . lines <$> getContents
  putStrLn "Bottom of the tower:"
  print $ findBase input
  putStrLn "Value required to be balenced:"
  print $ valueToCorrect =<< buildTree input
