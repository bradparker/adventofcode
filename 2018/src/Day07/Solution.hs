module Day07.Solution
  ( main
  ) where

import Control.Applicative (some)
import Control.Arrow ((&&&))
import Data.Char (chr, isAlpha, isUpper, ord)
import Data.Foldable (fold)
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import qualified Data.List as List
import Text.Megaparsec (Parsec, runParser)
import Text.Megaparsec.Char (newline, satisfy, string)

data Instruction = Instruction
  { dependancy :: Int
  , identifier :: Int
  } deriving (Show)

type Parser = Parsec String String

identifierParser :: Parser Int
identifierParser = ord <$> satisfy ((&&) <$> isAlpha <*> isUpper)

instructionParser :: Parser Instruction
instructionParser =
  Instruction
    <$> (string "Step " *> identifierParser)
    <*> (string " must be finished before step " *> identifierParser <* string " can begin.")

instructionsParser :: Parser [Instruction]
instructionsParser = some (instructionParser <* newline)

onParsedInput :: Show b => Parser a -> (a -> b) -> String -> String
onParsedInput parser act = either show (show . act) . runParser parser ""

type Steps = IntMap IntSet

steps :: [Instruction] -> Steps
steps =
  flip foldr IntMap.empty $ \ins ss ->
    IntMap.insertWith
      (<>)
      (identifier ins)
      (IntSet.singleton (dependancy ins))
      ss

roots :: Steps -> IntSet
roots ss = fold ss `IntSet.difference` IntMap.keysSet ss

rootSteps :: Steps -> Steps
rootSteps = IntSet.foldr (\s ss -> IntMap.insert s IntSet.empty ss) IntMap.empty . roots

withRoots :: Steps -> Steps
withRoots = uncurry (<>) . (rootSteps &&& id)

completeStep :: Int -> Steps -> Steps
completeStep step = IntMap.delete step . IntMap.map (IntSet.delete step)

performStep :: IntMap IntSet -> Maybe (Int, IntMap IntSet)
performStep queue =
  perform <$> IntMap.minViewWithKey (IntMap.filter IntSet.null queue)
  where
    perform ((next, _), _) = (next, completeStep next queue)

viewSteps :: IntMap IntSet -> [Int]
viewSteps = List.unfoldr performStep

partOne :: String -> String
partOne = onParsedInput instructionsParser (map chr . viewSteps . withRoots . steps)

main :: IO ()
main = do
  input <- readFile "src/Day07/input.txt"
  putStrLn $ partOne input
