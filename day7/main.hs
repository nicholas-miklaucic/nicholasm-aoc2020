module Main where

import Data.Matrix
import Data.List(nub, elemIndex)
import Data.Maybe(fromJust)

type Node = String
data Edge = Edge { src :: Node
                 , dest :: Node
                 , weight :: Int
                 } deriving Show
data Graph = Graph { nodes :: [Node]
                   , edges :: [Edge]
                   } deriving Show

parseGraph :: [String] -> Graph
parseGraph lines = Graph (parseNodes lines) (parseEdges lines)

parseNodes :: [String] -> [Node]
parseNodes = map parseNode

parseNode :: String -> Node
parseNode line = unwords $ take 2 $ words line

parseEdges :: [String] -> [Edge]
parseEdges = concatMap parseEdge

parseEdge :: String -> [Edge]
parseEdge line
  | srcsStr == ["no", "other", "bags."] = []
  | otherwise = map (makeEdge dest) srcs
  where
    dest = parseNode line
    srcsStr = drop 1 $ dropWhile (/="contain") $ words line
    srcs = splitInFours srcsStr

splitInFours :: [a] -> [(a, a, a, a)]
splitInFours [] = []
splitInFours (a:b:c:d:xs) = (a, b, c, d) : splitInFours xs

makeEdge :: Node -> (String, String, String, String) -> Edge
makeEdge dest (num, c1, c2, _) = Edge srcCol dest (read num) where
  srcCol = c1 ++ " " ++ c2

children :: Graph -> Node -> [Node]
children (Graph _ edges) node = [dest | Edge src dest _ <- edges, src == node]

allChildrenOfComp :: Graph -> [Node] -> [Node]
allChildrenOfComp graph currNodes
 | null currNodes = []
 | otherwise = nub (currNodes ++ newChildren ++ allChildrenOfComp graph newChildren)
  where
    newChildren = filter (`elem` nodes graph) $ nub $ concatMap (children graph) currNodes

allChildren :: Graph -> Node -> [Node]
allChildren graph node = allChildrenOfComp graph (children graph node)

graphToAdj :: Graph -> Matrix Int
graphToAdj (Graph nodes edges) = foldr (addEdge nodes) (zero (length nodes) (length nodes)) edges

addEdge :: [Node] -> Edge -> Matrix Int -> Matrix Int
addEdge nodes (Edge src dest weight) = unsafeSet weight (desti + 1, srci + 1) where
  srci = fromJust $ elemIndex src nodes
  desti = fromJust $ elemIndex dest nodes

isZero :: Matrix Int -> Bool
isZero mat = sum (toList mat) == 0

powersList :: Matrix Int -> Matrix Int -> [Matrix Int]
powersList exp base = f [] where
  f [] = f [exp * base]
  f list@(x:_)
    | isZero x = list
    | otherwise = f $ (x * base):list

getStartMat :: Graph -> String -> Matrix Int
getStartMat (Graph nodes _) node = rowVector $ getCol (i + 1) (identity (length nodes)) where
  i = fromJust $ elemIndex node nodes

totBags :: Graph -> String -> Int
totBags graph node = sum $ map (sum . toList) powers where
  row = getStartMat graph node
  powers = powersList row (graphToAdj graph)


main :: IO ()
main = do
  graph <- parseGraph . lines <$> readFile "input"
  let part1 = length $ allChildren graph "shiny gold"
  let part2 = totBags graph "shiny gold"
  putStr "Part 1: "
  print part1
  putStr "Part 2: "
  print part2
  -- let adj = graphToAdj graph
  -- print (powersList adj)
  -- print (length $ powersList adj)
