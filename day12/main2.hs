module Main where

data Action = N | E | S | W | L | R | F deriving (Enum, Eq, Show, Read)

type Posn = (Int, Int)

data Turtle = Turtle { pos :: Posn
                     , wayp :: Posn
                     } deriving (Eq, Show)

data Move = Move { action :: Action
                 , val :: Int
                 } deriving (Eq, Show)

parseMove :: String -> Move
parseMove (x:xs) = Move (read [x]) (read xs)

manhattanNorm :: Posn -> Int
manhattanNorm (x, y) = abs x + abs y

moveInDir :: Int -> Int -> Posn -> Posn
moveInDir deg val (x, y) = (x + dx, y + dy) where
  dx = round $ cos (fromIntegral deg * (pi / 180)) * fromIntegral val
  dy = round $ sin (fromIntegral deg * (pi / 180)) * fromIntegral val

getDeg :: Action -> Int
getDeg N = 90
getDeg E = 0
getDeg S = 270
getDeg W = 180
getDeg _ = error "Oops!"

posnAdd :: Posn -> Posn -> Posn
posnAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

posnSub :: Posn -> Posn -> Posn
posnSub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

rotate :: Int -> Posn -> Posn
rotate dir (x, y) = (round xnew, round ynew) where
  rad = fromIntegral dir * (pi / 180)
  xf = fromIntegral x
  yf = fromIntegral y
  xnew = xf * cos rad - yf * sin rad
  ynew = xf * sin rad + yf * cos rad

getNewPos :: Action -> Int -> Posn -> Posn -> Posn
getNewPos F val pos wayp = iterate (posnAdd wayp) pos !! val
getNewPos _ _ pos _ = pos

getNewWayp :: Action -> Int -> Posn -> Posn -> Posn
getNewWayp F _ _ wayp = wayp
getNewWayp L val _ wayp = rotate val wayp
getNewWayp R val pos wayp = getNewWayp L (360 - val) pos wayp
getNewWayp dir val _ wayp = moveInDir (getDeg dir) val wayp

moveBy :: Turtle -> Move -> Turtle
moveBy (Turtle pos wayp) (Move action val) = Turtle newPos newWayp where
  newPos = getNewPos action val pos wayp
  newWayp = getNewWayp action val pos wayp

part2Ans :: [Move] -> Int
part2Ans moves = manhattanNorm $ pos $ foldl moveBy (Turtle (0, 0) (10, 1)) moves

main :: IO ()
main = do
  moves <- map parseMove . lines <$> readFile "input"
  let part2 = part2Ans moves
  putStr "Part 2: "
  print part2
