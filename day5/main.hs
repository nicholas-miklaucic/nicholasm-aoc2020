module Main where

binPart :: (Char, Char) -> (Int, Int) -> String -> Int
binPart (f, b) (lo, hi) (h: rest)
  | f == h = binPart (f, b) (lo, hi - ((hi - lo + 1) `div` 2)) rest
  | b == h = binPart (f, b) (lo + ((hi - lo + 1) `div` 2), hi) rest
binPart (_, _) (lo, _) "" = lo
binPart _ _ _ = -1

findRow :: String -> Int
findCol :: String -> Int

findRow = binPart ('F', 'B') (0, 127)
findCol = binPart ('L', 'R') (0, 7)

findID :: String -> Int
findID str = row * 8 + col where
  (rowstr, colstr) = splitAt 7 str
  row = findRow rowstr
  col = findCol colstr

main :: IO ()
main = do
  seats <- map findID . lines <$> readFile "input"
  let part1 = maximum seats
  let part2 = head $ filter (\x -> (x `notElem` seats) && ((x + 1) `elem` seats) && ((x - 1) `elem` seats)) [minimum seats..maximum seats]
  putStr "Part 1: "
  print part1
  putStr "Part 2: "
  print part2
