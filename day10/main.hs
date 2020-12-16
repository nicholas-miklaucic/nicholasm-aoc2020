module Main where

import Data.List(sort)
import Data.Array.Unboxed

windows :: Int -> [a] -> [[a]]
windows n list
  | n > length list = []
  | n == length list = [list]
  | n < length list = take n list:windows n (drop 1 list)

part1Ans :: [Int] -> Int
part1Ans nums = a * b where
  (a, b) = foldr f (0, 1) $ windows 2 $ 0 : sort nums
  f [i, j] (a, b)
    | j == i + 1 = (a + 1, b)
    | j == i + 2 = (a, b)
    | j == i + 3 = (a, b + 1)
    | otherwise = error "Difference not in range 1-3"
  f _ _ = error "Bad inputs"

get :: (IArray a e, Ix i) => e -> a i e -> i -> e
get base arr ind = if ind `elem` indices arr then arr ! ind else base

part2Ans :: [Int] -> Int
part2Ans nums = f initCounts (0 : sort nums) where
  initCounts = (listArray (0, maximum nums) [0 | _ <- [0..maximum nums+1]]) // [(0, 1)]
  f :: UArray Int Int -> [Int] -> Int
  f counts [] = counts ! snd (bounds counts)
  f counts (0:xs) = f counts xs
  f counts (x:xs) = f newCounts xs where
    tryInd = get 0 counts
    newCounts = counts // [(x, tryInd (x - 1) + tryInd (x - 2) + tryInd (x - 3))]





main :: IO ()
main = do
  let nums = map read . lines <$> readFile "input"
  nums :: IO [Int]
  part1 <- part1Ans <$> nums
  putStr "Part 1: "
  print part1
  part2 <- part2Ans <$> nums
  putStr "Part 2: "
  print part2
