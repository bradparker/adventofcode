module Main (main) where

right :: Num a => (a, b) -> (a, b)
right (x, y) = (x + 1, y)

up :: Num b => (a, b) -> (a, b)
up (x, y) = (x, y + 1)

left :: Num a => (a, b) -> (a, b)
left (x, y) = (x - 1, y)

down :: Num b => (a, b) -> (a, b)
down (x, y) = (x, y - 1)

block :: Num a => Int -> [(a, a) -> (a, a)]
block k =
  replicate (2 * k + 1) right ++
  replicate (2 * k + 1) up ++
  replicate (2 * k + 2) left ++
  replicate (2 * k + 2) down

spiral :: Int -> [(Int, Int)]
spiral n = scanl (flip ($)) (0, 0) $ take (n - 1) $ foldMap block [0..]

taxiDistance :: (Int, Int) -> (Int, Int) -> Int
taxiDistance (a, b) (c, d) = abs (a - c) + abs (b - d)

distance :: Int -> Int
distance = taxiDistance (0, 0) . last . spiral

parseInt :: String -> Int
parseInt = read

main :: IO ()
main = do
  n <- parseInt <$> getLine
  print $ distance n
