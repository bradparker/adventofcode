module Main
  ( main
  ) where

import Data.Maybe
import Text.ParserCombinators.ReadP

parse :: String -> Maybe [Int]
parse =
  (fst <$>) .
  listToMaybe .
  readP_to_S
    (map read <$> (sepBy (many1 get) (char ',') <* eof))

revSection :: Int -> Int -> [a] -> [a]
revSection offset size xs =
  take
    len
    (drop
       (len - pos)
       (cycle
          (reverse (take size (drop pos (cycle xs))) ++
           take
             (len - size)
             (drop ((pos + size) `mod` len) (cycle xs)))))
  where
    len = length xs
    pos = offset `mod` len

tie :: ((Int, Int), [Int]) -> Int -> ((Int, Int), [Int])
tie ((position, skip), xs) size =
  ( (position + size + skip, skip + 1)
  , revSection position size xs)

knots :: [Int] -> [((Int, Int), [Int])]
knots = scanl tie ((0, 0), [0 .. 255])

solve :: [Int] -> Int
solve = product . take 2 . snd . last . knots

main :: IO ()
main = do
  input <- parse <$> getLine
  print $ solve <$> input
