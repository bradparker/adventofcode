{-# LANGUAGE LambdaCase #-}

module Main
  ( main,
  )
where

import Data.Functor.Foldable (ListF (Cons, Nil), para)
import Data.List (sort)

isAnagram :: String -> String -> Bool
isAnagram a b = sort a == sort b

anagramIn :: String -> [String] -> Bool
anagramIn word = any (isAnagram word)

hasAnyPairs :: [String] -> Bool
hasAnyPairs =
  para $ \case
    Nil -> False
    (Cons word (phrase, result)) ->
      (word `elem` phrase) || result

hasAnyAnagrams :: [String] -> Bool
hasAnyAnagrams =
  para $ \case
    Nil -> False
    (Cons word (phrase, result)) ->
      (word `anagramIn` phrase) || result

main :: IO ()
main = do
  phrases <- map words . lines <$> getContents
  putStrLn "Valid (equal)"
  print $ length $ filter not $ map hasAnyPairs phrases
  putStrLn "Valid (anagram)"
  print $ length $ filter not $ map hasAnyAnagrams phrases
