module Main
  ( main
  ) where

import Control.Monad
import Data.Bool
import Data.List.Zipper
import Data.Maybe

parseInt :: String -> Int
parseInt = read

parseInput :: String -> [Int]
parseInput = map parseInt . lines

intolerantLeft :: Zipper a -> Maybe (Zipper a)
intolerantLeft z = bool (Just (left z)) Nothing (beginp z)

intolerantRight :: Zipper a -> Maybe (Zipper a)
intolerantRight z =
  bool (Just (right z)) Nothing (endp (right z))

move :: Int -> Zipper a -> Maybe (Zipper a)
move n z =
  (foldr
     (<=<)
     return
     (replicate
        (abs n)
        (bool intolerantRight intolerantLeft (n < 0))))
    z

walk :: Int -> Zipper Int -> Int
walk steps z =
  case (move n zPosInc) of
    Nothing -> steps + 1
    Just zMoved -> walk (steps + 1) zMoved
  where
    n = cursor z
    n'' = bool (n + 1) (n - 1) (n >= 3)
    zPosInc = replace n'' z

main :: IO ()
main = do
  input <- parseInput <$> getContents
  print $ walk 0 (fromList input)
