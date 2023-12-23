{-# LANGUAGE TupleSections #-}

module Day23 (solve1, solve2, printGraph) where

import qualified Data.Array.Unboxed as A
import Data.List (nub)
import Data.List.Extra (maximumOn)
import qualified Data.Map as M
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text, unpack)
import qualified Text.Parsec as Parsec

-- Types

data Dir = TUp | TDown | TLeft | TRight deriving (Eq, Show, Ord)

data Tile = TPath | TForest | TSlope Dir deriving (Eq, Show, Ord)

type Pos = (Int, Int)

type Board = A.Array Pos Tile

-- Parsing

parseTile :: Char -> Tile
parseTile '.' = TPath
parseTile '#' = TForest
parseTile '>' = TSlope TRight
parseTile '<' = TSlope TLeft
parseTile '^' = TSlope TUp
parseTile 'v' = TSlope TDown

parseInput :: Text -> Board
parseInput ss = toBoard $ handleErr $ Parsec.parse inputData "Failure" $ unpack ss
  where
    inputData = Parsec.sepEndBy1 (map parseTile <$> Parsec.many1 (Parsec.noneOf "\n")) Parsec.endOfLine

    toBoard :: [[Tile]] -> Board
    toBoard xs = A.listArray ((0, 0), (length xs - 1, length (head xs) - 1)) (concat xs)

    handleErr (Left a) = error (show a)
    handleErr (Right a) = a

-- Part 1

solve1 :: Text -> Int
solve1 ss = (+ (-1)) . length $ longestPath board startPos endPos
  where
    board = parseInput ss
    startPos = head [p | p@(y, x) <- A.range (A.bounds board), y == 0, board A.! p == TPath]
    endPos = head [p | let (_, (yMax, _)) = A.bounds board, p@(y, x) <- A.range (A.bounds board), y == yMax, board A.! p == TPath]

movements :: Maybe Dir -> [Pos -> Pos]
movements (Just TUp) = [\(y, x) -> (y - 1, x)]
movements (Just TDown) = [\(y, x) -> (y + 1, x)]
movements (Just TLeft) = [\(y, x) -> (y, x - 1)]
movements (Just TRight) = [\(y, x) -> (y, x + 1)]
movements Nothing = concatMap movements [Just TUp, Just TDown, Just TLeft, Just TRight]

longestPath :: Board -> Pos -> Pos -> [Pos]
longestPath board start end = fromJust $ go [start] mempty
  where
    go :: [Pos] -> S.Set Pos -> Maybe [Pos]
    go path@(x : _) visited
      | x == end = Just path
      | S.member x visited = Nothing
      | not (A.inRange (A.bounds board) x) = Nothing
      | otherwise = case board A.! x of
          TForest -> Nothing
          TSlope dir -> go ((movements (Just dir) <*> [x]) ++ path) (S.insert x visited)
          TPath -> if null candidates then Nothing else Just $ maximumOn length candidates
            where
              candidates = mapMaybe (\p -> go (p : path) (S.insert x visited)) $ movements Nothing <*> [x]

-- Part 2

solve2 :: Text -> Int
solve2 ss = longestPath' adjacencyMap startPos endPos
  where
    board = parseInput ss
    startPos = head [p | p@(y, x) <- A.range (A.bounds board), y == 0, board A.! p == TPath]
    endPos = head [p | let (_, (yMax, _)) = A.bounds board, p@(y, x) <- A.range (A.bounds board), y == yMax, board A.! p == TPath]
    adjacencyMap = generateGraph board [startPos, endPos]

longestPath' :: M.Map Pos [(Pos, Int)] -> Pos -> Pos -> Int
longestPath' adjacencyMap start end = fromJust $ go (start, 0) mempty
  where
    go :: (Pos, Int) -> S.Set Pos -> Maybe Int
    go (x, l) visited
      | x == end = Just l
      | S.member x visited = Nothing
      | otherwise = if null candidates then Nothing else Just $ maximum candidates
      where
        candidates = mapMaybe (\(p, dl) -> go (p, l + dl) (S.insert x visited)) (adjacencyMap M.! x)

generateGraph :: Board -> [Pos] -> M.Map Pos [(Pos, Int)]
generateGraph board forcedNodes = adjacencyMap
  where
    nodes = forcedNodes ++ [p | p <- A.range (A.bounds board), board A.! p /= TForest, numPathsOut p >= 3]
      where
        numPathsOut pos = length . filter ((/= TForest) . (board A.!)) . filter (A.inRange (A.bounds board)) $ movements Nothing <*> [pos]
    edges = nub . map (\(n1, n2, d) -> (min n1 n2, max n1 n2, d)) $ concatMap (findEdges board (S.fromList nodes)) nodes
    adjacencyMap =
      M.fromList
        [ (n0, [(n1', l) | (n0', n1', l) <- edges, n0' == n0] ++ [(n1', l) | (n1', n0', l) <- edges, n0' == n0])
          | n0 <- nodes
        ]

    findEdges :: Board -> S.Set Pos -> Pos -> [(Pos, Pos, Int)]
    findEdges board nodes init = map (\(n, i) -> (init, n, i)) $ go [(init, 0)] S.empty
      where
        go :: [(Pos, Int)] -> S.Set Pos -> [(Pos, Int)]
        go [] _ = []
        go ((x, l) : xs) visited
          | S.member x nodes && x /= init = (x, l) : go xs (S.insert x visited)
          | otherwise = go (xs ++ map (,l + 1) newNodes) (S.insert x visited)
          where
            newNodes = filter (`S.notMember` visited) . filter ((/= TForest) . (board A.!)) . filter (A.inRange (A.bounds board)) $ movements Nothing <*> [x]

printGraph :: Text -> IO ()
printGraph ss = do
  putStrLn "graph {"
  mapM_ (putStrLn . (\n -> nodeName n ++ " [pos=\"" ++ nodePos n ++ "!\"]")) (M.keys adjacencyMap)
  mapM_ (putStrLn . (\(n0, n1, l) -> nodeName n0 ++ " -- " ++ nodeName n1 ++ " [label=" ++ show l ++ "]")) edges
  putStrLn "}"
  where
    board = parseInput ss
    startPos = head [p | p@(y, x) <- A.range (A.bounds board), y == 0, board A.! p == TPath]
    endPos = head [p | let (_, (yMax, _)) = A.bounds board, p@(y, x) <- A.range (A.bounds board), y == yMax, board A.! p == TPath]
    adjacencyMap = generateGraph board [startPos, endPos]
    edges = [(n0, n1, l) | n0 <- M.keys adjacencyMap, (n1, l) <- adjacencyMap M.! n0, n0 < n1]

    nodeName :: Pos -> String
    nodeName (i, j) = "n_" ++ show i ++ "_" ++ show j
    nodePos :: Pos -> String
    nodePos (i, j) = show (fromIntegral i / 10.0) ++ "," ++ show (10 - fromIntegral j / 10.0)
