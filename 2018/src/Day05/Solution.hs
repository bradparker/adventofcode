module Day05.Solution
  ( main
  ) where

import Control.Arrow ((&&&))
import Control.Monad.State (State, execState, get, put)
import Data.Char (isAlpha, toLower, toUpper)
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.List (minimumBy)

partOne :: String -> Int
partOne = length . flip execState [] . traverse_ stacker . filter isAlpha
  where
    stacker :: Char -> State String ()
    stacker a = do
      stack <- get
      case stack of
        [] -> put [a]
        (b:bs) ->
          if a /= b && (a == toUpper b || a == toLower b)
            then put bs
            else put $ a : b : bs

partTwo :: String -> Int
partTwo input =
  snd $
  minimumBy (on compare snd) $
  map (id &&& (\c -> partOne (filter ((/= c) . toLower) input))) ['a' .. 'z']

main :: IO ()
main = do
  input <- readFile "src/Day05/input.txt"
  putStrLn "Part one:"
  print $ partOne input
  putStrLn "Part two:"
  print $ partTwo input

