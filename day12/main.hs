module Main where

data Action = N | E | S | W | L | R | F deriving (Enum, Eq, Show, Read)

type Posn = (Int, Int)

data Turtle = Turtle { dir :: Int
                     , pos :: Posn
                     } deriving (Eq, Show)

data Move = Move { action :: Action
                 , val :: Int
                 } deriving (Eq, Show)

getMoveDeg :: Action -> Int -> Int
getMoveDeg N _ = 90
getMoveDeg E _ = 0
getMoveDeg S _ = 270
getMoveDeg W _ = 180
getMoveDeg L _ = 0
getMoveDeg R _ = 0
getMoveDeg F dir = dir

getNewDir :: Action -> Int -> Int -> Int
getNewDir L val dir = (dir + val) `mod` 360
getNewDir R val dir = (dir - val) `mod` 360
getNewDir _ _ dir = dir

moveInDir :: Int -> Int -> Posn -> Posn
moveInDir deg val (x, y) = (x + dx, y + dy) where
  dx = round $ cos (fromIntegral deg * (pi / 180)) * fromIntegral val
  dy = round $ sin (fromIntegral deg * (pi / 180)) * fromIntegral val

moveBy :: Move -> Turtle -> Turtle
moveBy (Move action val) (Turtle dir pos) = Turtle newDir newPos where
  moveDeg = getMoveDeg action dir
  newDir = getNewDir action val dir
  newPos = if action == L || action == R then pos else moveInDir moveDeg val pos

parseMove :: String -> Move
parseMove (x:xs) = Move (read [x]) (read xs)

manhattanNorm :: Posn -> Int
manhattanNorm (x, y) = abs x + abs y

part1Ans :: [Move] -> Int
part1Ans moves = manhattanNorm $ pos $ foldr moveBy (Turtle 0 (0, 0)) (reverse moves)

main :: IO ()
main = do
  moves <- map parseMove . lines <$> readFile "input"
  let part1 = part1Ans moves
  putStr "Part 1: "
  print part1
