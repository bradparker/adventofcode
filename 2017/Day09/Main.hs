module Main
  ( main
  ) where

import Text.ParserCombinators.Parsec

data Expression
  = Group [Expression]
  | Garbage String
  deriving (Show)

escaped :: Parser String
escaped = many1 (char '!' *> anyChar) *> return ""

content :: Parser String
content = many1 (noneOf "!>")

garbage :: Parser Expression
garbage =
  Garbage <$>
  between
    (char '<')
    (char '>')
    (concat <$> many (escaped <|> content))

group :: Parser Expression
group = Group <$> between (char '{') (char '}') expression

expression :: Parser [Expression]
expression = sepBy (group <|> garbage) (char ',')

score :: Expression -> Int
score ex = go 1 ex
  where
    go _ (Garbage _) = 0
    go par (Group []) = par
    go par (Group exs) =
      foldr (\e total -> go (par + 1) e + total) par exs

garbageLen :: Expression -> Int
garbageLen (Garbage chars) = length chars
garbageLen (Group []) = 0
garbageLen (Group exs) = sum (map garbageLen exs)

main :: IO ()
main = do
  input <- getContents
  putStrLn "Group score"
  print $
    (score <$>) <$>
    parse expression "" input
  putStrLn "Size of all cleaned garbage"
  print $
    (garbageLen <$>) <$>
    parse expression "" input
