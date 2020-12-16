module Main where

import Data.HashMap (Map)
import qualified Data.HashMap as Map
import Data.List
import Data.Maybe

data Seat = Empty | Occupied | Floor deriving Eq

instance Show Seat where
  show Empty = "L"
  show Occupied = "#"
  show Floor = "."

type Layout = Map (Int, Int) Seat

parseSeat :: Char -> Seat
parseSeat 'L' = Empty
parseSeat '#' = Occupied
parseSeat '.' = Floor
parseSeat x = error $ "Invalid character: " ++ [x]

indToCoord :: Int -> Int -> (Int, Int)
indToCoord n i = (i `div` n, i `mod` n)

parseLayout :: String -> Layout
parseLayout str = Map.fromList $ zipWith f [0..n*n-1] seats where
  n = length $ lines str
  seats = intercalate "" $ lines str
  f i seat = (indToCoord n i, parseSeat seat)

neighbors :: Layout -> (Int, Int) -> [Seat]
neighbors layout (i, j) = mapMaybe (\(x, y) -> Map.lookup (x, y) layout) adj where
  adj = [(i + di, j + dj) | di <- [-1, 0, 1], dj <- [-1, 0, 1], (di, dj) /= (0, 0)]

step :: Layout -> Layout
step layout = Map.mapWithKey newSeat layout where
  newSeat _ Floor = Floor
  newSeat (i, j) seat
    | seat == Occupied && numOccupied >= 4 = Empty
    | seat == Empty && numOccupied == 0 = Occupied
    | otherwise = seat
    where
      numOccupied = length $ filter (==Occupied) $ neighbors layout (i, j)

part1Ans :: Layout -> Int
part1Ans layout = f layout (step layout) where
  f prev curr =
    if prev == curr
    then length $ filter (==Occupied) $ Map.elems curr
    else f curr $ step curr

main :: IO ()
main = do
  layout <- parseLayout <$> readFile "input"
  putStr "Part 1: "
  print $ part1Ans layout
