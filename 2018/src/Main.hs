module Main where

import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout)

import qualified Day01.Solution as Day01
import qualified Day02.Solution as Day02
import qualified Day03.Solution as Day03
import qualified Day04.Solution as Day04
import qualified Day05.Solution as Day05
import qualified Day06.Solution as Day06
import qualified Day07.Solution as Day07

runDay :: Int -> IO () -> IO ()
runDay number day = do
  putStrLn $ "Day " <> show number
  day *> putStr "\n"

runDays :: [IO ()] -> IO ()
runDays = foldMap (uncurry runDay) . zip [1..]

main :: IO ()
main =  do
  hSetBuffering stdout LineBuffering

  runDays
    [ Day01.main
    , Day02.main
    , Day03.main
    , Day04.main
    , Day05.main
    , Day06.main
    , Day07.main
    ]
