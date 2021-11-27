module Day04.Solution
  ( main,
  )
where

import Control.Applicative (many, some, (<|>))
import Control.Arrow (second)
import Control.Monad.State (evalState, get, put)
import Data.Bool (bool)
import Data.Function (on)
import Data.Functor (void)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (maximumBy, sortOn)
import Data.Traversable (for)
import Text.Megaparsec (Parsec, runParser, try)
import Text.Megaparsec.Char (char, digitChar, newline, string)

type Parser = Parsec String String

integerP :: Parser Int
integerP = read <$> some digitChar

timeP :: Parser Time
timeP = do
  void $ char '['
  y <- integerP
  void $ char '-'
  mon <- integerP
  void $ char '-'
  d <- integerP
  void $ char ' '
  h <- integerP
  void $ char ':'
  m <- integerP
  void $ char ']'
  pure $ Time y mon d h m

awakeP :: Parser Bool
awakeP = False <$ string "falls asleep" <|> True <$ string "wakes up"

sleepP :: Parser LogEntry
sleepP = do
  t <- timeP
  void $ char ' '
  LogEntry t . Awake <$> awakeP

shiftStartP :: Parser LogEntry
shiftStartP = do
  t <- timeP
  void $ char ' '
  LogEntry t . ShiftStart
    <$> (string "Guard #" *> integerP <* string " begins shift")

logEntryP :: Parser LogEntry
logEntryP = try sleepP <|> shiftStartP

logP :: Parser [LogEntry]
logP = many (logEntryP <* newline)

data LogEntry = LogEntry
  { time :: Time,
    event :: Event
  }
  deriving (Show)

data Event
  = Awake Bool
  | ShiftStart Int
  deriving (Show)

isSleepEvent :: Event -> Bool
isSleepEvent (Awake _) = True
isSleepEvent _ = False

data Time = Time
  { year :: Int,
    month :: Int,
    day :: Int,
    hour :: Int,
    minute :: Int
  }
  deriving (Show, Eq, Ord)

shiftsByGuard :: [LogEntry] -> IntMap [[Bool]]
shiftsByGuard = go IntMap.empty
  where
    go m [] = m
    go m (l : ls) =
      case l of
        LogEntry _ (Awake _) -> go m ls
        LogEntry _ (ShiftStart i) -> go m' next
          where
            (sleepLogs, next) = span (isSleepEvent . event) ls
            shiftSleep = expandBinaryMap True 59 (sleepMap sleepLogs)
            m' = IntMap.insertWith (++) i [shiftSleep] m

sleepMap :: [LogEntry] -> IntMap Bool
sleepMap =
  flip foldr IntMap.empty $ \(LogEntry t e) m ->
    case e of
      Awake a -> IntMap.insert (minute t) a m
      _ -> m

expandBinaryMap :: Bool -> Int -> IntMap Bool -> [Bool]
expandBinaryMap s n m =
  flip evalState s $
    for [0 .. n] $ \i -> maybe (pure ()) put (IntMap.lookup i m) *> get

frequency :: [[Bool]] -> IntMap Int
frequency =
  foldr (uncurry (IntMap.insertWith (+))) IntMap.empty
    . concatMap (zip [0 ..] . map (bool 1 0))

sleepFrequencyByGuard :: IntMap [[Bool]] -> IntMap (IntMap Int)
sleepFrequencyByGuard = IntMap.map frequency

sleepiestGuardsFrequency :: IntMap [[Bool]] -> (Int, IntMap Int)
sleepiestGuardsFrequency =
  maximumBy (on compare (sum . snd)) . IntMap.toList . sleepFrequencyByGuard

keyWithLargestValue :: IntMap Int -> Int
keyWithLargestValue = fst . maximumBy (on compare snd) . IntMap.toList

partOne :: String -> String
partOne =
  either
    show
    ( show
        . uncurry (*)
        . second keyWithLargestValue
        . sleepiestGuardsFrequency
        . shiftsByGuard
        . sortOn time
    )
    . runParser logP "Advent of Code: Day 04"

partOneMain :: IO ()
partOneMain = putStrLn . partOne =<< readFile "src/Day04/input.txt"

guardWithHighestFrequency :: IntMap [[Bool]] -> (Int, IntMap Int)
guardWithHighestFrequency =
  maximumBy (on compare (maximum . snd)) . IntMap.toList . sleepFrequencyByGuard

partTwo :: String -> String
partTwo =
  either
    show
    ( show
        . uncurry (*)
        . second keyWithLargestValue
        . guardWithHighestFrequency
        . shiftsByGuard
        . sortOn time
    )
    . runParser logP "Advent of Code: Day 04"

partTwoMain :: IO ()
partTwoMain = putStrLn . partTwo =<< readFile "src/Day04/input.txt"

main :: IO ()
main = do
  putStrLn "Part one:"
  partOneMain
  putStrLn "Part two:"
  partTwoMain
