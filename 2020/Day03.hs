{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Applicative (many)
import Control.Arrow ((***))
import Control.Monad (guard)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.State (State, evalState, get, put)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Array (Array, array, bounds, (!))
import Data.Bifunctor (first)
import Data.Function ((&))

type Position = (Int, Int)

type GridSquare = Bool -- False = Empty & True = Tree

type Grid = Array Position GridSquare

gridSquare :: Char -> GridSquare
gridSquare '#' = True
gridSquare _ = False

grid :: [String] -> Grid
grid rows =
  array ((0, 0), (width, height)) do
    (y, row) <- zip [0 ..] rows
    (x, val) <- zip [0 ..] row
    return ((x, y), gridSquare val)
  where
    width = maximum (map length rows) - 1
    height = length rows - 1

type Journey a = MaybeT (ReaderT Grid (State Position)) a

move :: Int -> Int -> Journey ()
move dx dy = do
  (maxX, maxY) <- asks (snd . bounds)
  (x, y) <- get
  let y' = dy + y
  guard (y' <= maxY)
  put ((x + dx) `mod` (maxX + 1), y')

amOnTree :: Journey Bool
amOnTree = asks (!) <*> get

countTreesOnRoute :: Int -> Int -> Journey Int
countTreesOnRoute dx dy =
  length . filter id <$> many do
    move dx dy
    amOnTree

evalJourney :: Grid -> Position -> Journey a -> Maybe a
evalJourney g p = (`evalState` p) . (`runReaderT` g) . runMaybeT

countingTrees :: Grid -> Int -> Int -> Maybe Int
countingTrees g dx dy = evalJourney g (0, 0) (countTreesOnRoute dx dy)

parseInput :: String -> Grid
parseInput = grid . lines

solveTersely :: Array (Int, Int) Bool -> (Int, Int) -> Int
solveTersely forest (dx, dy) =
  -- generate infinite grid positions
  iterate ((+ dx) *** (+ dy)) (0, 0)
    -- ensure that the right moves 'wrap'
    & map (first (`mod` (fst (snd (bounds forest)) + 1)))
    -- stop producing when we go off the bottom
    & takeWhile ((<= snd (snd (bounds forest))) . snd)
    -- read the grid at every position
    & map (forest !)
    -- filter out empty cells
    & filter id
    -- count
    & length

main :: IO ()
main = do
  g <- parseInput <$> getContents
  print $ countingTrees g 3 1
  print $ product <$> traverse (uncurry (countingTrees g)) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
  print $ solveTersely g (3, 1)
  print $ product (map (solveTersely g) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)])
