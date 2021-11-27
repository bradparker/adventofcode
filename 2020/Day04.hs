{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Applicative (some, (<|>))
import Control.Monad (guard, replicateM)
import Data.Char (isDigit, isSpace)
import Data.Foldable (asum)
import Data.Functor (void)
import Data.Maybe (isJust)
import Parser
  ( Parser,
    char,
    eof,
    parse,
    satisfy,
    string,
  )
import Text.Read (readMaybe)

type Field = (String, String)

type Object = [Field]

space :: Parser ()
space = void (satisfy isSpace)

fieldP :: Parser Field
fieldP = (,) <$> chars <* char ':' <*> chars <* (space <|> eof)
  where
    chars = some (satisfy (not . isSpace))

newline :: Parser ()
newline = void (char '\n')

objectP :: Parser Object
objectP = some fieldP <* (newline <|> eof)

objectsP :: Parser [Object]
objectsP = some objectP <* eof

parseInput :: String -> Maybe [Object]
parseInput = parse objectsP

isValid :: Object -> Bool
isValid e =
  all
    (isJust . (`lookup` e))
    [ "byr",
      "iyr",
      "eyr",
      "hgt",
      "hcl",
      "ecl",
      "pid"
    ]

number :: Parser Int
number = do
  digits <- some (satisfy isDigit)
  case readMaybe digits of
    Nothing -> fail "No parse"
    Just n -> pure n

year :: Parser Int
year = do
  digits <- replicateM 4 (satisfy isDigit)
  case readMaybe digits of
    Nothing -> fail "No parse"
    Just n -> pure n

-- four digits; at least 1920 and at most 2002.
newtype BirthYear = BirthYear Int
  deriving (Show)

decodeBirthYear :: String -> Maybe BirthYear
decodeBirthYear str = do
  n <- parse (year <* eof) str
  guard (n >= 1920 && n <= 2002)
  pure (BirthYear n)

-- four digits; at least 2010 and at most 2020.
newtype IssueYear = IssueYear Int
  deriving (Show)

decodeIssueYear :: String -> Maybe IssueYear
decodeIssueYear str = do
  n <- parse (year <* eof) str
  guard (n >= 2010 && n <= 2020)
  pure (IssueYear n)

-- four digits; at least 2020 and at most 2030.
newtype ExpirationYear = ExpirationYear Int
  deriving (Show)

decodeExpirationYear :: String -> Maybe ExpirationYear
decodeExpirationYear str = do
  n <- parse (year <* eof) str
  guard (n >= 2020 && n <= 2030)
  pure (ExpirationYear n)

-- | a number followed by either cm or in:
-- If cm, the number must be at least 150 and at most 193.
-- If in, the number must be at least 59 and at most 76.
data Unit = Inches | Centimeters
  deriving (Show)

data Height = Height Int Unit
  deriving (Show)

decodeHeight :: String -> Maybe Height
decodeHeight str = do
  (n, u) <- parse ((,) <$> number <*> unit <* eof) str
  case u of
    Centimeters -> do
      guard (n >= 150 && n <= 193)
      pure (Height n u)
    Inches -> do
      guard (n >= 59 && n <= 76)
      pure (Height n u)
  where
    unit :: Parser Unit
    unit = (Centimeters <$ string "cm") <|> (Inches <$ string "in")

-- a # followed by exactly six characters 0-9 or a-f.
newtype HairColor = HairColor String
  deriving (Show)

decodeHairColor :: String -> Maybe HairColor
decodeHairColor =
  parse (HairColor <$> hex <* eof)
  where
    hex :: Parser String
    hex = (:) <$> char '#' <*> replicateM 6 hexDigit
    hexDigit :: Parser Char
    hexDigit = satisfy isDigit <|> satisfy (`elem` ['a' .. 'f'])

-- exactly one of: amb blu brn gry grn hzl oth.
data EyeColor
  = AMB
  | BLU
  | BRN
  | GRY
  | GRN
  | HZL
  | OTH
  deriving (Show)

decodeEyeColor :: String -> Maybe EyeColor
decodeEyeColor =
  parse
    ( asum
        [ AMB <$ string "amb",
          BLU <$ string "blu",
          BRN <$ string "brn",
          GRY <$ string "gry",
          GRN <$ string "grn",
          HZL <$ string "hzl",
          OTH <$ string "oth"
        ]
    )

-- a nine-digit number, including leading zeroes.
newtype PassportID = PassportID String
  deriving (Show)

decodePassportID :: String -> Maybe PassportID
decodePassportID = (PassportID <$>) . parse (replicateM 9 (satisfy isDigit) <* eof)

-- ignored, missing or not.
newtype CountryID = CountryID String
  deriving (Show)

decodeCountryID :: String -> Maybe CountryID
decodeCountryID = Just . CountryID

data Passport = Passport
  { byr :: BirthYear,
    iyr :: IssueYear,
    eyr :: ExpirationYear,
    hgt :: Height,
    hcl :: HairColor,
    ecl :: EyeColor,
    pid :: PassportID,
    cid :: Maybe CountryID
  }
  deriving (Show)

field :: String -> (String -> Maybe a) -> Object -> Maybe a
field name valueDecoder object = valueDecoder =<< lookup name object

optionalField :: String -> (String -> Maybe a) -> Object -> Maybe (Maybe a)
optionalField name valueDecoder object =
  case lookup name object of
    Nothing -> Just Nothing
    Just v -> Just <$> valueDecoder v

decodePassport :: Object -> Maybe Passport
decodePassport object =
  Passport
    <$> field "byr" decodeBirthYear object
    <*> field "iyr" decodeIssueYear object
    <*> field "eyr" decodeExpirationYear object
    <*> field "hgt" decodeHeight object
    <*> field "hcl" decodeHairColor object
    <*> field "ecl" decodeEyeColor object
    <*> field "pid" decodePassportID object
    <*> optionalField "cid" decodeCountryID object

main :: IO ()
main = do
  Just objects <- parseInput <$> getContents
  print $ length (filter id (map isValid objects))
  print $ length (filter isJust (map decodePassport objects))
