module Crypto.Hash.Knot
  ( encode
  , knotted
  ) where

import Data.Bits (xor)
import Data.Char (ord)
import Data.Foldable (foldl')
import Data.List.Split (chunksOf)
import Numeric (showHex)

slice :: Int -> Int -> [a] -> [a]
slice offset size = take size . drop offset

revSection :: Int -> Int -> [a] -> [a]
revSection offset size xs =
  slice
    (len - pos)
    len
    (cycle
       (reverse (slice pos size (cycle xs)) ++
        slice
          ((pos + size) `mod` len)
          (len - size)
          (cycle xs)))
  where
    len = length xs
    pos = offset `mod` len

tie :: ((Int, Int), [Int]) -> Int -> ((Int, Int), [Int])
tie ((position, skip), xs) size =
  ( (position + size + skip, skip + 1)
  , revSection position size xs)

knotted :: [Int] -> [Int]
knotted = snd . foldl' tie ((0, 0), [0 .. 255])

leftPad :: Int -> a -> [a] -> [a]
leftPad targetLength pad xs =
  replicate (targetLength - length xs) pad ++ xs

encode :: String -> String
encode =
  concatMap ((leftPad 2 '0' . (`showHex` "")) . foldr1 xor) .
  chunksOf 16 .
  knotted .
  concat .
  replicate 64 . (++ [17, 31, 73, 47, 23]) . map ord
