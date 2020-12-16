module Main where

hasSubsetSum :: [Int] -> Int -> Bool
hasSubsetSum set n = or [n == a + b && a /= b | a <- set, b <- set]

windows :: Int -> [a] -> [[a]]
windows n list
  | n > length list = []
  | n == length list = [list]
  | n < length list = take n list:windows n (drop 1 list)

failures :: Int -> [Int] -> [Int]
failures n nums = reverse $ map head $ filter (\lon -> not $ hasSubsetSum (tail lon) (head lon)) $ windows (n+1) $ reverse nums

findPairDiffs :: [Int] -> Int -> (Int, Int)
findPairDiffs nums solSum = head $ map fst $ filter ((==solSum) . snd)  [((i, j), a - b) | (i, a) <- enum, (j, b) <- enum] where
  enum = zip [0..length nums - 1] (scanr (+) 0 nums)

findWindowSum :: [Int] -> Int -> [Int]
findWindowSum nums solSum = take (j - i) $ drop (i + 1) nums where
  (i, j) = findPairDiffs nums solSum


getPart2Ans :: [Int] -> Int
getPart2Ans nums = minimum nums + maximum nums

main :: IO ()
main = do
  nums <- map read . lines <$> readFile "input"
  let part1 = head $ failures 25 nums
  let part2Nums = findWindowSum (dropWhile (>=part1) $ reverse nums) part1
  let part2 = getPart2Ans part2Nums
  putStr "Part 1: "
  print part1
  putStr "Part 2: "
  print part2
  print part2Nums
