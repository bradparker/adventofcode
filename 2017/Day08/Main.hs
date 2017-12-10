module Main (main) where

import Text.ParserCombinators.ReadP (ReadP, readP_to_S, munch1, char, string)
import Control.Applicative ((<|>))
import Data.Char (isAlpha, isDigit)
import Data.Maybe (listToMaybe)
import Data.Map (Map, findWithDefault, empty, insert, toList)
import Data.Bool (bool)
import Control.Lens (view, _2)

type Operation = ((Int -> Int -> Int), String, Int)
type Comparason = ((Int -> Int -> Bool), String, Int)
type Instruction = (Operation, Comparason)

intP :: ReadP Int
intP = read <$> ((++) <$> (string "-" <|> return "") <*> munch1 isDigit)

registerP :: ReadP String
registerP = munch1 isAlpha

operatorP :: ReadP (Int -> Int -> Int)
operatorP =
  (string "inc" *> return (+)) <|>
  (string "dec" *> return (-))

compOperatorP :: ReadP (Int -> Int -> Bool)
compOperatorP =
  (string "> " *> return (>)) <|>
  (string "< " *> return (<)) <|>
  (string "== " *> return (==)) <|>
  (string ">= " *> return (>=)) <|>
  (string "<= " *> return (<=)) <|>
  (string "!= " *> return (/=))

comparasonP :: ReadP Comparason
comparasonP = do
  _ <- string "if "
  reg <- registerP
  _ <- char ' '
  op <- compOperatorP
  val <- intP
  return (op, reg, val)

operationP :: ReadP Operation
operationP = do
  reg <- registerP
  _ <- char ' '
  op <- operatorP
  _ <- char ' '
  val <- intP
  return (op, reg, val)

instructionP :: ReadP Instruction
instructionP = do
  operation <- operationP
  _ <- char ' '
  comparason <- comparasonP
  return (operation, comparason)

parseInstruction :: String -> Maybe Instruction
parseInstruction = (fst <$>) . listToMaybe . readP_to_S instructionP

parseInstructions :: String -> Maybe [Instruction]
parseInstructions = mapM parseInstruction . lines

runOperation :: ((Int -> Int -> b), String, Int) -> Map String Int -> b
runOperation (op, key, operand) m = op (findWithDefault 0 key m) operand

runInstruction :: Instruction -> Map String Int -> Map String Int
runInstruction (op, comp) m = bool m (insert (view _2 op) (runOperation op m) m) (runOperation comp m)

runInstructions :: [Instruction] -> [Map String Int]
runInstructions = scanl (flip runInstruction) empty

main :: IO ()
main = do
  input <- parseInstructions <$> getContents
  putStrLn "Maximum at last state:"
  print $ maximum . map snd . toList . last . runInstructions <$> input
  putStrLn "Maximum of any state:"
  print $ maximum . ((map snd . toList) =<<) . runInstructions <$> input
