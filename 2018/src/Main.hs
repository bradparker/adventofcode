module Main where

import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout)

import qualified Day01.Solution as Day01
import qualified Day02.Solution as Day02
import qualified Day03.Solution as Day03
import qualified Day04.Solution as Day04
import qualified Day05.Solution as Day05
import qualified Day06.Solution as Day06

main :: IO ()
main =  do
  hSetBuffering stdout LineBuffering

  putStrLn "Day 01"
  Day01.main
  putStrLn ""
  putStrLn "Day 02"
  Day02.main
  putStrLn ""
  putStrLn "Day 03"
  Day03.main
  putStrLn ""
  putStrLn "Day 04"
  Day04.main
  putStrLn ""
  putStrLn "Day 05"
  Day05.main
  putStrLn ""
  putStrLn "Day 06"
  Day06.main
