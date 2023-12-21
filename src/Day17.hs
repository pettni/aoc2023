{-# LANGUAGE MultiWayIf #-}

module Day17 (solve1, solve2) where

import Control.Arrow ((>>>))
import qualified Data.Array.Unboxed as A
import Data.Char (ord)
import qualified Data.Heap as H
import qualified Data.Set as S
import Data.Text (Text, unpack)
import Data.Tuple (swap)
import qualified Text.Parsec as Parsec

-- Types

data Dir = TS | TN | TE | TW deriving (Eq, Show, Ord)

type Pos = (Int, Int)

data State = State {pos :: Pos, dir :: Dir, stepsInDir :: Int} deriving (Eq, Ord)

data StateWithCost = StateWithCost {state :: State, cost :: Int} deriving (Eq, Ord)

type Board = A.Array Pos Int

-- Parsing

parseInput :: Text -> Board
parseInput ss = toBoard $ handleErr $ Parsec.parse inputData "Failure" $ unpack ss
  where
    inputData = Parsec.sepEndBy1 intLine Parsec.endOfLine
    intLine = map (\x -> ord x - ord '0') <$> Parsec.many1 (Parsec.noneOf "\n")

    toBoard xs = A.listArray ((0, 0), (length xs - 1, length (head xs) - 1)) (concat xs)

    handleErr (Left a) = error (show a)
    handleErr (Right a) = a

-- Part 1

opposite :: Dir -> Dir
opposite TN = TS
opposite TS = TN
opposite TE = TW
opposite TW = TE

step :: Pos -> Dir -> Pos
step (x, y) TS = (x, y + 1)
step (x, y) TN = (x, y - 1)
step (x, y) TW = (x - 1, y)
step (x, y) TE = (x + 1, y)

pathFind :: Board -> Int -> Int -> State -> (State -> Bool) -> Int
pathFind board minStep maxStep init goalCond = pathFind' mempty (H.singleton (0, StateWithCost init 0))
  where
    (_, (yMax, xMax)) = A.bounds board

    pathFind' :: S.Set (Pos, Dir, Int) -> H.MinPrioHeap Int StateWithCost -> Int
    pathFind' close open = case H.view open of
      Nothing -> error "No solution found"
      Just ((_, StateWithCost s@(State pos dir stepsInDir) runningCost), openN) ->
        if
          | goalCond s && stepsInDir >= minStep -> runningCost
          | S.member (pos, dir, stepsInDir) close -> pathFind' close openN
          | otherwise -> pathFind' (S.insert (pos, dir, stepsInDir) close) (H.union newStates openN)
        where
          newStates =
            H.fromList
              [ (newRunningCost + heuristic newState, StateWithCost newState newRunningCost)
                | dirOut <- [TN, TS, TW, TE],
                  dirOut /= opposite dir,
                  let posOut = step pos dirOut,
                  A.inRange (A.bounds board) $ swap posOut,
                  let stepsInDirOut = if dir == dirOut then stepsInDir + 1 else 1,
                  (dirOut /= dir) || (stepsInDirOut <= maxStep),
                  (dirOut == dir) || (stepsInDir >= minStep),
                  let newState = State posOut dirOut stepsInDirOut,
                  let newRunningCost = runningCost + board A.! swap posOut
              ]
          heuristic (State (x, y) _ _) = abs (x - xMax) + abs (y - yMax)

solve :: Int -> Int -> Text -> Int
solve minStep maxStep ss = pathFind board minStep maxStep (State (0, 0) TE 0) (pos >>> swap >>> (== snd (A.bounds board)))
  where
    board = parseInput ss

solve1 :: Text -> Int
solve1 = solve 0 3

-- Part 2

solve2 :: Text -> Int
solve2 = solve 4 10
