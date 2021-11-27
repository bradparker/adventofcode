{-# OPTIONS_GHC -Wall #-}

module Parser
  ( module ReadP,
    Parser,
    alpha,
    digit,
    natural,
    newline,
    parse,
    space,
  )
where

import Control.Applicative (some)
import Data.Char (isAlpha, isDigit)
import Data.Functor (void)
import Data.Maybe (listToMaybe)
import Numeric.Natural (Natural)
import Text.ParserCombinators.ReadP as ReadP

parse :: Parser a -> String -> Maybe a
parse p = fmap fst . listToMaybe . readP_to_S p

type Parser a = ReadP a

space :: Parser ()
space = void (char ' ')

alpha :: Parser Char
alpha = satisfy isAlpha

newline :: Parser ()
newline = void (char '\n')

digit :: Parser Char
digit = satisfy isDigit

natural :: Parser Natural
natural = read <$> some digit
