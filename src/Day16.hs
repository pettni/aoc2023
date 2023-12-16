module Day16 (solve1, solve2) where

import qualified Data.Set as S
import Data.Text (Text, unpack)
import qualified Text.Parsec as Parsec

-- Types

data Dir = TS | TN | TE | TW deriving (Eq, Show, Ord)

type Pos = (Int, Int)

type State = (Pos, Dir)

-- Parsing

parseInput :: Text -> [[Char]]
parseInput ss = handleErr $ Parsec.parse inputData "Failure" $ unpack ss
  where
    inputData = Parsec.sepEndBy1 (Parsec.many1 (Parsec.noneOf "\n")) Parsec.endOfLine
    handleErr (Left a) = error (show a)
    handleErr (Right a) = a

-- Part 1

solve1 :: Text -> Int
solve1 ss = length $ S.map fst visited
  where
    board = parseInput ss
    startState = ((0, 0), TE)
    visited = traversal board mempty $ S.singleton startState

traversal :: [[Char]] -> S.Set State -> S.Set State -> S.Set State
traversal board visited toVisit
  | S.null toVisit = visited
  | otherwise = traversal board (S.insert s visited) (S.union (S.difference newStates visited) (S.deleteAt 0 toVisit))
  where
    s@(p@(x, y), dir) = S.elemAt 0 toVisit
    newStates = S.fromList [(newPos, newDir) | newDir <- newDirs, let newPos = step newDir p, inRange newPos]
      where
        newDirs = inOut dir (board !! y !! x)
        inRange (x, y) = y >= 0 && y < length board && x >= 0 && x < length (board !! y)

step :: Dir -> Pos -> Pos
step TE (x, y) = (x + 1, y)
step TW (x, y) = (x - 1, y)
step TN (x, y) = (x, y - 1)
step TS (x, y) = (x, y + 1)

inOut :: Dir -> Char -> [Dir]
inOut d '.' = [d]
inOut TE '-' = [TE]
inOut TW '-' = [TW]
inOut TN '-' = [TW, TE]
inOut TS '-' = [TW, TE]
inOut TS '|' = [TS]
inOut TN '|' = [TN]
inOut TE '|' = [TS, TN]
inOut TW '|' = [TS, TN]
inOut TN '\\' = [TW]
inOut TE '\\' = [TS]
inOut TW '\\' = [TN]
inOut TS '\\' = [TE]
inOut TN '/' = [TE]
inOut TE '/' = [TN]
inOut TW '/' = [TS]
inOut TS '/' = [TW]

-- Part 2

solve2 :: Text -> Int
solve2 ss = maximum $ length . S.map fst . traversal board mempty . S.singleton <$> startStates
  where
    board = parseInput ss
    nRows = length board
    nCols = length $ head board
    startStates =
      [((0, y), TE) | y <- [0 .. nRows - 1]]
        ++ [((nCols - 1, y), TW) | y <- [0 .. nRows - 1]]
        ++ [((x, 0), TS) | x <- [0 .. nCols - 1]]
        ++ [((x, nRows - 1), TN) | x <- [0 .. nCols - 1]]
