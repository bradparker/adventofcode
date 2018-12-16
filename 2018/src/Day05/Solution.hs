module Day05.Solution
  ( main
  ) where

import Data.Group (invert)
import Data.Algebra.Free (returnFree, foldMapFree)
import Data.Group.Free (FreeGroupL, toList)
import Data.Char (isAlpha, toLower, isUpper, isLower)

inject :: Char -> FreeGroupL Char
inject c
  | isAlpha c && isLower c = returnFree c
  | isAlpha c && isUpper c = invert $ returnFree $ toLower c
  | otherwise = mempty

partOne :: String -> Int
partOne = length . toList . foldMap inject

clean :: Char -> FreeGroupL Char -> FreeGroupL Char
clean c = foldMapFree $ \d ->
  if d == c
    then mempty
    else returnFree d

partTwo :: String -> Int
partTwo input =
  minimum $ map (length . toList . (`clean` foldMap inject input)) ['a' .. 'z']

main :: IO ()
main = do
  input <- readFile "src/Day05/input.txt"
  putStrLn "Part one:"
  print $ partOne input
  putStrLn "Part two:"
  print $ partTwo input

