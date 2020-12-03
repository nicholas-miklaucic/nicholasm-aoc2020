module Main where

type Field = [[Bool]]

parseLine :: String -> [Bool]
parseLine = map f where
  f '.' = False
  f '#' = True
  f _ = undefined

parseField :: String -> Field
parseField str = map parseLine $ lines str

isTreeAt :: Field -> (Int, Int) -> Bool
isTreeAt field (x, y) = (field !! y) !! x

numTreesHit :: Field -> (Int, Int) -> Int
numTreesHit field (dx, dy) = length $ filter (isTreeAt field) posns where
  height = length field
  width = length $ head field
  f :: (Int, Int) -> (Int, Int)
  f (x, y) = ((x + dx) `mod` width, y + dy)
  posns = takeWhile (\(_, y) -> y < height) $ iterate f (0, 0)

main :: IO ()
main = do
  input <- parseField <$> readFile "input.txt"
  putStr "Part 1: "
  print $ numTreesHit input (1, 2)
  let slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
  putStr "Part 2: "
  print $ product $ fmap (numTreesHit input) slopes
