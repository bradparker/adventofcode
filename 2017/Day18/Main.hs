{-# LANGUAGE NamedFieldPuns #-}

module Main
  ( main
  ) where

import Control.Arrow ((&&&))
import Data.Foldable (length)
import Data.Map (Map, findWithDefault, insert, insertWith, empty)
import Data.Vector (Vector, (!), fromList)
import Data.List (unfoldr)
import Data.Char (isAlpha)
import Data.Bool (bool)
import Data.Maybe (listToMaybe, catMaybes)

parseExp :: String -> Expression
parseExp exp = bool (Value (read exp)) (Variable (head exp)) (isAlpha (head exp))

parseLine :: [String] -> Statement
parseLine ["snd", expression] = Play (parseExp expression)
parseLine ["set", register, expression] = Set (head register) (parseExp expression)
parseLine ["add", register, expression] = Add (head register) (parseExp expression)
parseLine ["mul", register, expression] = Multiply (head register) (parseExp expression)
parseLine ["mod", register, expression] = Modulo (head register) (parseExp expression)
parseLine ["rcv", expression] = Recover (parseExp expression)
parseLine ["jgz", a, b] = Jump (parseExp a) (parseExp b)

data Expression
  = Value Int
  | Variable Register
  deriving Show

data Statement
  = Play Expression
  | Recover Expression
  | Set Register
        Expression
  | Add Register
        Expression
  | Multiply Register
            Expression
  | Modulo Register
           Expression
  | Jump Expression
         Expression
  deriving Show

-- A Stack for outbox
-- A Stack for inbox
-- A Map for registers
-- An Array for instructions
type Message = Int

type Register = Char

type Environment = Map Register Int

type Line = Int

data Program = Program
  { line :: Line
  , outbox :: [Message]
  , inbox :: [Maybe Message]
  , environment :: Environment
  , instructions :: Vector Statement
  }
  deriving Show

evaluate :: Expression -> Environment -> Int
evaluate (Value a) _ = a
evaluate (Variable a) env = findWithDefault 0 a env

set :: Register -> Expression -> Program -> Program
set register expression duet@Program {environment, line} =
  duet
  { environment =
      insert
        register
        (evaluate expression environment)
        environment
  , line = line + 1
  }

add :: Register -> Expression -> Program -> Program
add register expression duet@Program {environment = env, line} =
  duet
  { environment =
      insert
        register
        (evaluate (Variable register) env + evaluate expression env)
        env
  , line = line + 1
  }

multiply :: Register -> Expression -> Program -> Program
multiply register expression duet@Program {environment = env, line} =
  duet
  { environment =
      insert
        register
        (evaluate (Variable register) env * evaluate expression env)
        env
  , line = line + 1
  }

modulo :: Register -> Expression -> Program -> Program
modulo register expression duet@Program {environment = env, line} =
  duet
  { environment =
      insert
        register
        (evaluate (Variable register) env `mod` evaluate expression env)
        env
  , line = line + 1
  }

jump :: Expression -> Expression -> Program -> Program
jump expressionX expressionY duet@Program {line, environment}
  | x > 0 = duet {line = line + y}
  | otherwise = duet {line = line + 1}
  where
    x = evaluate expressionX environment
    y = evaluate expressionY environment

play :: Expression -> Program -> Program
play expression duet@Program {outbox, environment, line} =
  duet
  { outbox = evaluate expression environment : outbox
  , line = line + 1
  }

recover :: Expression -> Program -> Program
recover expression duet@Program { outbox
                             , inbox
                             , line
                             , environment
                             }
  | evaluate expression environment == 0 =
    duet {line = line + 1}
  | otherwise =
    duet
    {inbox = listToMaybe outbox : inbox, line = line + 1}

step :: Program -> Maybe Program
step duet@Program {line, instructions}
  | line >= length instructions = Nothing
  | otherwise =
    case instructions ! line of
      Play expression -> Just (play expression duet)
      Recover expression -> Just (recover expression duet)
      Set register expression ->
        Just (set register expression duet)
      Add register expression ->
        Just (add register expression duet)
      Multiply register expression ->
        Just (multiply register expression duet)
      Modulo register expression ->
        Just (modulo register expression duet)
      Jump expressionX expressionY ->
        Just (jump expressionX expressionY duet)

perform :: [Statement] -> [Program]
perform = unfoldr (((id &&& id) <$>) . step) . (Program 0 [] [] empty) . fromList

solve1 :: [Statement] -> Maybe Int
solve1 = (listToMaybe =<<) . listToMaybe . filter ((> 0) . length) . map (catMaybes . inbox) . perform

main :: IO ()
main = do
  prog <- map (parseLine . words) . lines <$> getContents
  print $ solve1 prog
