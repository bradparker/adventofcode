module Main
  ( main
  ) where

import Control.Lens (view, _3)
import Control.Monad (void)
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (listToMaybe, mapMaybe)
import Text.Megaparsec
import Text.Megaparsec.Lexer (decimal, signed)

type Point = (Int, Int, Int)

type Velocity = (Int, Int, Int)

type Acceleration = (Int, Int, Int)

type Particle = (Point, Velocity, Acceleration)

distance :: Point -> Point -> Int
distance (x1, y1, z1) (x2, y2, z2) =
  sum (map abs (zipWith (-) [x2, y2, z2] [x1, y1, z1]))

number :: (Num a) => Parsec () String a
number = fromIntegral <$> decimal

signedNumber :: (Num a) => Parsec () String a
signedNumber = signed (void (optional spaceChar)) number

vec3 :: Parsec () String (Int, Int, Int)
vec3 =
  between
    (char '<')
    (char '>')
    ((,,) <$> signedNumber <*> (char ',' *> signedNumber) <*>
     (char ',' *> signedNumber))

point :: Parsec () String Point
point = string "p=" *> vec3

velocity :: Parsec () String Velocity
velocity = string "v=" *> vec3

acceleration :: Parsec () String Acceleration
acceleration = string "a=" *> vec3

particle :: Parsec () String Particle
particle =
  (,,) <$> point <*> (string ", " *> velocity) <*>
  (string ", " *> acceleration)

parseParticle :: String -> Maybe Particle
parseParticle = parseMaybe particle

parseInput :: String -> [Particle]
parseInput = mapMaybe parseParticle . lines

slowestAcc :: [Particle] -> Maybe (Int, Particle)
slowestAcc =
  listToMaybe .
  sortBy (on compare (distance (0, 0, 0) . view _3 . snd)) .
  zip [0 ..]

main :: IO ()
main = do
  input <- parseInput <$> getContents
  putStrLn "Particle with the slowest acceleration:"
  print (slowestAcc input)
