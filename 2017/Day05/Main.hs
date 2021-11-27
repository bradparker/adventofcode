module Main
  ( main,
  )
where

import Control.Monad
import Data.Bool
import Data.List.Zipper

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
move n =
  foldr
    (<=<)
    return
    ( replicate
        (abs n)
        (bool intolerantRight intolerantLeft (n < 0))
    )

replaceA :: Num a => a -> Zipper a -> Zipper a
replaceA n = replace (n + 1)

replaceB :: (Ord a, Num a) => a -> Zipper a -> Zipper a
replaceB n = replace (bool (n + 1) (n - 1) (n >= 3))

walk ::
  (Int -> Zipper Int -> Zipper Int) -> Zipper Int -> Int
walk replacer zipper = go 0 (Just zipper)
  where
    go acc Nothing = acc
    go acc (Just z) =
      go
        (acc + 1)
        ((\n -> move n (replacer n z)) =<< cursor z)

main :: IO ()
main = do
  input <- parseInput <$> getContents
  print $ walk replaceA (fromList input)
  print $ walk replaceB (fromList input)
