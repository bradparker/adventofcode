{-# LANGUAGE NamedFieldPuns #-}

module Main
  ( main
  ) where

import           Control.Arrow ((&&&))
import           Control.Monad ((<=<))
import           Data.Bool (bool)
import           Data.Char (isAlpha)
import           Data.Foldable (length)
import           Data.List (unfoldr)
import           Data.Map
  (Map, empty, findWithDefault, insert)
import qualified Data.Map as Map
import           Data.Maybe (listToMaybe)
import           Data.Vector (Vector, fromList, (!))

parseExp :: String -> Expression
parseExp expression =
  bool
    (Value (read expression))
    (Variable (head expression))
    (isAlpha (head expression))

parseLineA :: [String] -> Statement
parseLineA ["snd", expression] = Play (parseExp expression)
parseLineA ["set", register, expression] =
  Set (head register) (parseExp expression)
parseLineA ["add", register, expression] =
  Add (head register) (parseExp expression)
parseLineA ["mul", register, expression] =
  Multiply (head register) (parseExp expression)
parseLineA ["mod", register, expression] =
  Modulo (head register) (parseExp expression)
parseLineA ["rcv", expression] =
  Recover (parseExp expression)
parseLineA ["jgz", a, b] = Jump (parseExp a) (parseExp b)
parseLineA _ = error "Unrecognised instruction"

parseLineB :: [String] -> Statement
parseLineB ["snd", expression] = Send (parseExp expression)
parseLineB ["rcv", expression] = Receive (head expression)
parseLineB a = parseLineA a

data Expression
  = Value Int
  | Variable Register
  deriving (Show)

data Statement
  = Play Expression
  | Recover Expression
  | Send Expression
  | Receive Register
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
  deriving (Show)

type Message = Int

type Register = Char

type Environment = Map Register Int

type Line = Int

data Program = Program
  { line :: Line
  , outbox :: [Message]
  , inbox :: [Message]
  , env :: Environment
  , instructions :: Vector Statement
  } deriving (Show)

evaluate :: Expression -> Environment -> Int
evaluate (Value a) _ = a
evaluate (Variable a) env = findWithDefault 0 a env

set :: Register -> Expression -> Program -> Program
set register expression program@Program {env, line} =
  program
  { env = insert register (evaluate expression env) env
  , line = line + 1
  }

add :: Register -> Expression -> Program -> Program
add register expression program@Program {env = env, line} =
  program
  { env =
      insert
        register
        (evaluate (Variable register) env +
         evaluate expression env)
        env
  , line = line + 1
  }

multiply :: Register -> Expression -> Program -> Program
multiply register expression program@Program { env = env
                                             , line
                                             } =
  program
  { env =
      insert
        register
        (evaluate (Variable register) env *
         evaluate expression env)
        env
  , line = line + 1
  }

modulo :: Register -> Expression -> Program -> Program
modulo register expression program@Program {env = env, line} =
  program
  { env =
      insert
        register
        (evaluate (Variable register) env `mod`
         evaluate expression env)
        env
  , line = line + 1
  }

jump :: Expression -> Expression -> Program -> Program
jump expressionX expressionY program@Program {line, env}
  | x > 0 = program {line = line + y}
  | otherwise = program {line = line + 1}
  where
    x = evaluate expressionX env
    y = evaluate expressionY env

play :: Expression -> Program -> Program
play expression program@Program {outbox, env, line} =
  program
  { outbox = evaluate expression env : outbox
  , line = line + 1
  }

recover :: Expression -> Program -> Program
recover expression program@Program { outbox
                                   , inbox
                                   , line
                                   , env
                                   }
  | evaluate expression env == 0 = program {line = line + 1}
  | null outbox = program
  | otherwise =
    program {inbox = head outbox : inbox, line = line + 1}

stepSolo :: Program -> Maybe Program
stepSolo program@Program {line, instructions}
  | line >= length instructions = Nothing
  | otherwise =
    Just $
    case instructions ! line of
      Play expression -> play expression program
      Recover expression -> recover expression program
      Set register expression ->
        set register expression program
      Add register expression ->
        add register expression program
      Multiply register expression ->
        multiply register expression program
      Modulo register expression ->
        modulo register expression program
      Jump expressionX expressionY ->
        jump expressionX expressionY program

performSolo :: [Statement] -> [Program]
performSolo =
  unfoldr (((id &&& id) <$>) . stepSolo) .
  Program 0 [] [] empty . fromList

solve1 :: [Statement] -> Maybe Int
solve1 =
  (listToMaybe =<<) .
  listToMaybe .
  filter (not . null) . map inbox . performSolo

send ::
     Expression -> (Program, Program) -> (Program, Program)
send expression (progA@Program {env, line, outbox}, progB@Program {inbox}) =
  ( progA
    { line = line + 1
    , outbox = evaluate expression env : outbox
    }
  , progB {inbox = evaluate expression env : inbox})

receive ::
     Register -> (Program, Program) -> (Program, Program)
receive register (progA@Program {env, line, inbox}, progB)
  | null inbox = (progA, progB)
  | otherwise =
    ( progA
      { env = insert register (last inbox) env
      , inbox = init inbox
      , line = line + 1
      }
    , progB)

step' :: (Program, Program) -> Maybe (Program, Program)
step' (progA@Program {line, instructions}, progB)
  | line >= length instructions = Nothing
  | otherwise =
    Just $
    case instructions ! line of
      Send expression -> send expression (progA, progB)
      Receive register -> receive register (progA, progB)
      Set register expression ->
        (set register expression progA, progB)
      Add register expression ->
        (add register expression progA, progB)
      Multiply register expression ->
        (multiply register expression progA, progB)
      Modulo register expression ->
        (modulo register expression progA, progB)
      Jump expressionX expressionY ->
        (jump expressionX expressionY progA, progB)

flipTuple :: (a, b) -> (b, a)
flipTuple = snd &&& fst

stepDuet :: (Program, Program) -> Maybe (Program, Program)
stepDuet = (flipTuple <$>) . step' . flipTuple <=< step'

performDuet :: [Statement] -> [(Program, Program)]
performDuet =
  unfoldr (((id &&& id) <$>) . stepDuet) .
  (Program 0 [] [] (Map.fromList [('p', 0)]) &&&
   Program 0 [] [] (Map.fromList [('p', 1)])) .
  fromList

isDeadlock ::
     (Program, Program) -> (Program, Program) -> Bool
isDeadlock (a, b) (a', b') =
  line a == line a' && line b == line b'

solve2 :: [Statement] -> Int
solve2 =
  length .
  outbox .
  snd .
  fst .
  last .
  takeWhile (not . uncurry isDeadlock) .
  uncurry zip . (id &&& tail) . performDuet

main :: IO ()
main = do
  (progA, progB) <-
    (map (parseLineA . words) . lines &&&
     map (parseLineB . words) . lines) <$>
    getContents
  print $ solve1 progA
  print $ solve2 progB
