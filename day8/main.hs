module Main where

data Op = Nop Int
  | Jmp Int
  | Acc Int
  deriving Show

parseOp :: String -> Op
parseOp ('n':'o':'p':' ':xs) = Nop (readSignInt xs)
parseOp ('a':'c':'c':' ':xs) = Acc (readSignInt xs)
parseOp ('j':'m':'p':' ':xs) = Jmp (readSignInt xs)
parseOp _ = error "Something went wrong parsing"

readSignInt :: String -> Int
readSignInt ('+':num) = read num
readSignInt ('-':num) = -(read num)
readSignInt _ = error "Error in parsing number"

parseLines :: String -> [Op]
parseLines str = map parseOp $ lines str

data State = State { program :: [Op]
                   , currI :: Int
                   , acc :: Int
                   , hist :: [Int]
                   }
             deriving Show

initState :: String -> State
initState = opsToState . parseLines

opsToState :: [Op] -> State
opsToState ops = State ops 0 0 []

step :: State -> State
step (State ops currI acc hist)
  | nextI `elem` hist = State ops currI acc hist
  | nextI == length ops = State ops nextI acc hist
  | otherwise = step (State ops nextI nextAcc newHist)
  where
    newHist = currI:hist
    currOp = ops !! currI
    nextI = case currOp of
      Nop _ -> currI + 1
      Jmp inc -> currI + inc
      Acc _ -> currI + 1
    nextAcc = case currOp of
      Nop _ -> acc
      Jmp _ -> acc
      Acc inc -> acc + inc

hasTerminated :: State -> Bool
hasTerminated (State ops i _ _) = i == length ops

flipOp :: Op -> Op
flipOp (Nop i) = Jmp i
flipOp (Jmp i) = Nop i
flipOp x = x

flipOpAt :: [Op] -> Int -> [Op]
flipOpAt [] _ = []
flipOpAt (op:ops) i
  | i == 0 = flipOp op:ops
  | otherwise = op:flipOpAt ops (i - 1)

main :: IO ()
main = do
  state <- initState <$> readFile "input"
  let part1 = acc $ step state
  let part2 = acc $ head $ filter hasTerminated $ map (step . opsToState . flipOpAt (program state)) [0..length (program state) - 1]
  putStr "Part 1: "
  print part1
  putStr "Part 2: "
  print part2
