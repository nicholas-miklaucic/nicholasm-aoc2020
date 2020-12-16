module Main where

import Data.List.Split
import Data.Maybe
import Debug.Trace

part1Ans :: Integer -> [Integer] -> Integer
part1Ans start ids = busId * wait where
  (busId, wait) = foldr1 cmp $ map (compWait start) ids
  compWait start i = (i, -start `mod` (-i))
  cmp x@(_, w1) y@(_, w2) = if w1 <= w2 then x else y

readId :: String -> Maybe Integer
readId "x" = Nothing
readId str = Just $ read str


-- Returns the first solution of
-- t == a1 (mod b1)
-- t == a2 (mod b2)
-- for the list [(a1, b1), (a2, b2),]
solveSystem :: [(Integer, Integer)] -> Integer
solveSystem [] = error "Cannot solve empty system"
solveSystem [(a, b)] = if a /= 0 then a else a + b
solveSystem ((a1, b1):(a2, b2):xs) = solveSystem (combine (a1, b1) (a2, b2):xs)

combine :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
combine (a1, b1) (a2, b2) = ((a1 * m2 * b2 + a2 * m1 * b1) `mod` (b1 * b2), b1 * b2) where
  (m1, m2) = bezout b1 b2

bezout :: Integer -> Integer -> (Integer, Integer)
bezout a b = (olds, oldt) where
  (_, _, olds, _, oldt, _) = egcd (a, b, 1, 0, 0, 1)
  egcd (oldr, 0, olds, s, oldt, t) = (oldr, 0, olds, s, oldt, t)
  egcd (oldr, r, olds, s, oldt, t) = egcd (r, oldr - q * r, s, olds - q * s, t, oldt - q * t) where
    q = oldr `div` r

readSystem :: [String] -> [(Integer, Integer)]
-- I promise I write good code...sometimes...
readSystem ids = map (\(x, y) -> (toInteger (-x + 1), y)) $
  filter (\(_, y) -> y /= 0) $
  zip [1..length ids+1] $ map f ids where
  f "x" = 0
  f str = read str

main :: IO ()
main = do
  [timeLine, idLine] <- take 2 . lines <$> readFile "input"
  let start = read timeLine
  let ids = mapMaybe readId $ splitOn "," idLine
  let system = readSystem $ splitOn "," idLine
  let part1 = part1Ans start ids
  let part2 = solveSystem system
  putStr "Part 1: "
  print part1
  putStr "Part 2: "
  print part2
