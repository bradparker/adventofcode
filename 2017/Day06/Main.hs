module Main
  ( main
  ) where

import           Data.Bool (bool)
import           Data.List (elemIndex)
import           Data.Map
  ( Map
  , foldrWithKey
  , fromList
  , insert
  , insertWith
  , size
  )
import qualified Data.Set as Set

parseInt :: String -> Int
parseInt = read

maxKeyValue :: Ord b => Map a b -> Maybe (a, b)
maxKeyValue = foldrWithKey go Nothing
  where
    go k v Nothing = Just (k, v)
    go k v (Just (mk, mv)) =
      bool (Just (mk, mv)) (Just (k, v)) (v >= mv)

distribute :: (Int, Int) -> Map Int Int -> Map Int Int
distribute (index, value) registers
  | value > 0 =
    distribute
      (shiftRight index registers, value - 1)
      (insertWith (+) index 1 registers)
  | otherwise = registers

resetMax :: Int -> Map Int Int -> Map Int Int
resetMax index = insert index 0

shiftRight :: Int -> Map b c -> Int
shiftRight index = mod (index + 1) . size

redistributeMax :: Map Int Int -> Maybe (Map Int Int)
redistributeMax input = do
  (k, v) <- maxKeyValue input
  return
    (distribute (shiftRight k input, v) (resetMax k input))

redistributionCycles :: Map Int Int -> [Maybe (Map Int Int)]
redistributionCycles input =
  scanl (>>=) (return input) (repeat redistributeMax)

takeWhileUnique :: Ord a => [a] -> [a]
takeWhileUnique = go Set.empty []
  where
    go _ acc [] = acc
    go s acc (x:xs)
      | x `Set.member` s = acc
      | otherwise = go (Set.insert x s) (x : acc) xs

numUntilRepeated :: [Int] -> Int
numUntilRepeated =
  length .
  takeWhileUnique .
  redistributionCycles . fromList . zip [0 ..]

loopSize :: [Int] -> Maybe Int
loopSize registers =
  (length unique -) <$> elemIndex repeated cycles
  where
    unique = takeWhileUnique cycles
    cycles =
      redistributionCycles (fromList (zip [0 ..] registers))
    repeated = cycles !! length unique

main :: IO ()
main = do
  input <- ((parseInt <$>) . words) <$> getLine
  putStrLn "Cycles until repeated step: "
  print $ numUntilRepeated input
  putStrLn "Size of repeated loop: "
  print $ loopSize input
