{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Applicative (some, (<|>))
import Control.Arrow ((&&&))
import Control.Monad.State (evalState, get, modify)
import Data.Function ((&))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.List (find, unfoldr)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Parser
  ( Parser,
    char,
    eof,
    natural,
    newline,
    parse,
    space,
    string,
  )

data Instruction
  = Acc Int
  | Jmp Int
  | Nop Int
  deriving (Show)

instructionP :: Parser Instruction
instructionP = op <*> (space *> (sign <*> (fromIntegral <$> natural)))
  where
    sign :: Parser (Int -> Int)
    sign = (* 1) <$ char '+' <|> (* (-1)) <$ char '-'
    op :: Parser (Int -> Instruction)
    op = Acc <$ string "acc" <|> Jmp <$ string "jmp" <|> Nop <$ string "nop"

type Program = IntMap Instruction

parseInput :: String -> Maybe Program
parseInput =
  fmap (IntMap.fromList . zip [0 ..])
    . parse (some (instructionP <* (newline <|> eof)) <* eof)

move :: Instruction -> Int
move ins =
  case ins of
    Acc _ -> 1
    Nop _ -> 1
    Jmp v -> v

indexes :: Program -> [Int]
indexes program = unfoldr step 0
  where
    step :: Int -> Maybe (Int, Int)
    step current =
      (current,) . ((current +) . move)
        <$> IntMap.lookup current program

takeWhileM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
takeWhileM _ [] = return []
takeWhileM p (x : xs) = do
  q <- p x
  if q
    then takeWhileM p xs >>= (return . (:) x)
    else return []

takeWhileUnique :: Ord a => [a] -> [a]
takeWhileUnique =
  flip evalState Set.empty . takeWhileM \n -> do
    ns <- get
    modify (Set.insert n)
    pure (not (Set.member n ns))

-- This is a bit slower ... interesting
-- takeWhileUnique' :: Eq a => [a] -> [a]
-- takeWhileUnique' as =
--   case as of
--     [] -> []
--     (a : as') -> a : takeWhileUnique' (takeWhile (/= a) as')

delta :: Instruction -> Int
delta ins =
  case ins of
    Acc v -> v
    Nop _ -> 0
    Jmp _ -> 0

deltas :: Program -> [Int] -> [Int]
deltas program = mapMaybe (fmap delta . (`IntMap.lookup` program))

run :: Program -> Int
run program = sum (deltas program (takeWhileUnique (indexes program)))

repair :: Program -> Maybe (Program, [Int])
repair program =
  program
    & IntMap.keys
    & map (\k -> IntMap.adjust change k program)
    & map (id &&& (takeWhileUnique . indexes))
    & find ((lastIndex ==) . maximum . snd)
  where
    lastIndex :: Int
    lastIndex = IntSet.findMax (IntMap.keysSet program)

    change :: Instruction -> Instruction
    change instruction =
      case instruction of
        Nop v -> Jmp v
        Jmp v -> Nop v
        Acc v -> Acc v

run' :: Program -> Maybe Int
run' = fmap (sum . uncurry deltas) . repair

main :: IO ()
main = do
  Just program <- parseInput <$> getContents
  print (run program)
  print (run' program)
