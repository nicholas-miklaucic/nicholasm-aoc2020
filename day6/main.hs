module Main where

import Data.List.Split

allLetters = "abcdefghijklmnopqrstuvwxyz"

readGroups :: String -> [[String]]
readGroups str = splitOn [""] $ lines str

groupCount1 :: [String] -> Int
groupCount1 people = length $ filter isPresent allLetters where
  isPresent letter = any (letter `elem`) people

groupCount2 :: [String] -> Int
groupCount2 people = length $ filter isPresentEverywhere allLetters where
  isPresentEverywhere letter = all (letter `elem`) people

main :: IO ()
main = do
  groups <- readGroups <$> readFile "input"
  let part1 = sum $ map groupCount1 groups
  putStr "Part 1: "
  print part1
  let part2 = sum $ map groupCount2 groups
  putStr "Part 2: "
  print part2
