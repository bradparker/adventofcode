module Main (main) where

import Data.Functor.Foldable
  (ListF (Cons, Nil), para)
import Data.Set (fromList)

isAnagram :: String -> String -> Bool
isAnagram a b = fromList a == fromList b

anagramIn :: String -> [String] -> Bool
anagramIn word words = any (isAnagram word) words

hasAnyPairs :: [String] -> Bool
hasAnyPairs = para pair
  where
    pair Nil = False
    pair (Cons word (words, result)) = (word `elem` words) || result

hasAnyAnagrams :: [String] -> Bool
hasAnyAnagrams = para anagram
  where
    anagram Nil = False
    anagram (Cons word (words, result)) = (word `anagramIn` words) || result

main :: IO ()
main = do
  phrases <- (map words . lines) <$> getContents
  putStrLn "Valid (equal)"
  print $ length $ filter not $ map hasAnyPairs phrases
  putStrLn "Valid (anagram)"
  print $ length $ filter not $ map hasAnyAnagrams phrases
