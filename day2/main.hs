module Main where

data Req = Req Int Int Char deriving Show

parseReq :: String -> Req
parseReq str = Req low hi letter where
  (lowstr, histr) = break (=='-') $ takeWhile (/=' ') str
  low = read lowstr
  hi = read $ drop 1 histr
  letter = last str


meetsReq1 :: String -> Req -> Bool
meetsReq1 str (Req low hi letter) = let count = length $ filter (==letter) str in
  (low <= count) && (count <= hi)

validPassword :: (String -> Req -> Bool) -> String -> Bool
validPassword satisfies str = password `satisfies` req
  where
    (reqStr, passStr) = break (==':') str
    req = parseReq reqStr
    password = drop 2 passStr

meetsReq2 :: String -> Req -> Bool
meetsReq2 str (Req low hi letter) = atLow /= atHi where
  atLow = letter == (str !! (low - 1))
  atHi = letter == (str !! (hi - 1))

main :: IO ()
main = do
  input <- readFile "input.txt"
  let ans1 = part1 $ lines input
  let ans2 = part2 $ lines input
  putStrLn "Part 1:"
  print ans1
  putStrLn "Part 2:"
  print ans2

part1 :: [String] -> Int
part2 :: [String] -> Int
part1 lines = length $ filter (validPassword meetsReq1) lines
part2 lines = length $ filter (validPassword meetsReq2) lines
