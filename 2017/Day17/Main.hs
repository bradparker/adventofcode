module Main
  ( main,
  )
where

import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.List (elemIndices)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

insertAt :: Int -> a -> Seq a -> Seq a
insertAt pos value xs =
  uncurry
    ((<>) . (<> Seq.singleton value))
    (Seq.splitAt pos xs)

step :: (Int, Seq Int) -> (Int, Int) -> (Int, Seq Int)
step (_, xs) (pos, value) = (pos, insertAt pos value xs)

spinlock :: Int -> Int -> (Int, Seq Int)
spinlock iterations interval =
  foldl'
    step
    (0, Seq.empty)
    (zip (positions interval) [0 .. iterations])

positions :: Int -> [Int]
positions interval =
  scanl
    (\prev size -> ((prev + interval) `mod` size) + 1)
    0
    [1 ..]

solve1 :: Int -> Int
solve1 =
  uncurry (flip Seq.index) . first (+ 1) . spinlock 2017

solve2 :: Int -> Int
solve2 = last . elemIndices 1 . take 50000000 . positions

main :: IO ()
main = do
  input <- read <$> getLine
  print $ solve1 input
  print $ solve2 input
