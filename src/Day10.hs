module Day10 (solve1, solve2, partitionAdjacent, Orientation (..)) where

import Control.Exception (throw)
import Data.List (find)
import Data.List.Extra ((!?))
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust, isJust, mapMaybe)
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import Data.Tuple (swap)
import qualified Text.Parsec as Parsec

-- Types

data Orientation = ON | OS | OW | OE deriving (Show, Eq)

data State = State ((Int, Int), Orientation) | Send deriving (Show, Eq)

-- Parsing

parseInput :: Text -> [[Char]]
parseInput ss = handleErr $ Parsec.parse inputData "Failure" $ T.unpack ss
  where
    inputData = Parsec.sepEndBy1 line Parsec.endOfLine
    line = Parsec.many1 (Parsec.noneOf "\n")

    handleErr (Left a) = error (show a)
    handleErr (Right a) = a

-- Part 1

nextOr :: Orientation -> Char -> Maybe Orientation
nextOr ON '|' = Just ON
nextOr OS '|' = Just OS
nextOr OW '-' = Just OW
nextOr OE '-' = Just OE
nextOr OS 'L' = Just OE
nextOr OW 'L' = Just ON
nextOr OS 'J' = Just OW
nextOr OE 'J' = Just ON
nextOr OE '7' = Just OS
nextOr ON '7' = Just OW
nextOr ON 'F' = Just OE
nextOr OW 'F' = Just OS
nextOr _ _ = Nothing

stepDelta :: Orientation -> (Int, Int)
stepDelta o = case o of
  ON -> (-1, 0)
  OS -> (1, 0)
  OE -> (0, 1)
  OW -> (0, -1)

dynamics :: [[Char]] -> State -> Maybe State
dynamics _ Send = Nothing
dynamics t (State ((i0, j0), o0)) = if symbol == Just 'S' then Just Send else getNext (symbol >>= nextOr o0) (i1, j1)
  where
    (i1, j1) = (i0 + di, j0 + dj)
      where
        (di, dj) = stepDelta o0

    symbol = (t !? i1) >>= (!? j1)

    getNext :: Maybe Orientation -> (Int, Int) -> Maybe State
    getNext (Just newOr) newPos = Just (State (newPos, newOr))
    getNext _ _ = Nothing

-- Note: does not account for infinite loops that don't contain Send
createTrajectory :: (State -> Maybe State) -> State -> [State]
createTrajectory t s0 = if isJust s1 then s0 : createTrajectory t (fromJust s1) else [s0]
  where
    s1 = t s0

getMainLoop :: [[Char]] -> [State]
getMainLoop tiles = head . filter (\x -> last x == Send) $ createTrajectory (dynamics tiles) <$> startStates
  where
    startIdx = head [(i, j) | i <- [0 .. length tiles - 1], j <- [0 .. length (tiles !! i) - 1], tiles !! i !! j == 'S']
    startStates =
      [ State (startIdx, OS),
        State (startIdx, ON),
        State (startIdx, OW),
        State (startIdx, OE)
      ]

solve1 :: Text -> Int
solve1 ss = (`div` 2) . (\x -> x - 1) . length . getMainLoop $ parseInput ss

-- Part 2

-- go through main loop ('x') and mark adjacent '.' tiles with 'L' or 'R' depending on side
allDeltasCCW =
  [ (-1, -1),
    (0, -1),
    (1, -1),
    (1, 0),
    (1, 1),
    (0, 1),
    (-1, 1),
    (-1, 0)
  ]

partitionAdjacent :: Orientation -> Orientation -> ([(Int, Int)], [(Int, Int)])
partitionAdjacent oin oout = (right, left)
  where
    (i0_prev, j0_prev) = stepDelta oin
    delIn = (-i0_prev, -j0_prev)
    delOut = stepDelta oout

    deltasStart = drop 1 . dropWhile (/= delIn) $ cycle allDeltasCCW
    right = takeWhile (/= delOut) deltasStart
    deltasEnd = drop 1 . dropWhile (/= delOut) $ deltasStart
    left = takeWhile (/= delIn) deltasEnd

type Seq2d a = Seq.Seq (Seq.Seq a)

index2d :: Int -> Int -> Seq2d a -> a
index2d i j m = Seq.index (Seq.index m i) j

in2d :: Int -> Int -> Seq2d a -> Bool
in2d i j m = i >= 0 && i < Seq.length m && j >= 0 && j < Seq.length (Seq.index m i)

update2d :: Int -> Int -> a -> Seq2d a -> Seq2d a
update2d i j v m = Seq.update i (Seq.update j v (Seq.index m i)) m

flatten2d :: Seq2d a -> Seq.Seq a
flatten2d = foldl (Seq.><) mempty

solve2 :: Text -> Int
solve2 ss = Seq.length $ Seq.filter (== innerChar) $ flatten2d filledLrTileMap
  where
    tiles = parseInput ss
    nRows = length tiles
    nCols = length (head tiles)

    mainLoop = getMainLoop tiles

    -- main loop indices
    mainLoopIdx = S.fromList $ (\(State (idx, or)) -> idx) <$> (reverse . drop 1 . reverse) mainLoop

    -- store new map with four values ('x', '.', 'L', 'R')
    initTileMap = Seq.fromList [Seq.fromList [if S.member (i, j) mainLoopIdx then 'x' else '.' | j <- [0 .. nCols - 1]] | i <- [0 .. nRows - 1]]

    -- fill 'L' and 'R' to left and right of the mainLoop
    lrTileMap = foldl foldFn initTileMap $ zip (drop 1 mainLoop) mainLoop
      where
        foldFn :: Seq2d Char -> (State, State) -> Seq2d Char
        foldFn m (State ((i, j), oout), State (_, oin)) = (\m -> foldl (fUpdateInner 'L') m leftDel) $ foldl (fUpdateInner 'R') m rghtDel
          where
            (rghtDel, leftDel) = partitionAdjacent oin oout

            fUpdateInner :: Char -> Seq2d Char -> (Int, Int) -> Seq2d Char
            fUpdateInner v m' (di, dj)
              | not $ in2d i' j' m' = m'
              | index2d i' j' m' == '.' = update2d i' j' v m'
              | otherwise = m'
              where
                (i', j') = (i + di, j + dj)
        foldFn m (Send, _) = m

    -- fast marching to cover all '.'
    filledLrTileMap = head $ dropWhile (elem '.' . flatten2d) filledTileMaps
      where
        filledTileMaps = scanl foldFn lrTileMap [0 ..]
        foldFn :: Seq2d Char -> Int -> Seq2d Char
        foldFn m _ = foldl fUpdateInner m [(i, j) | i <- [0 .. nRows - 1], j <- [0 .. nCols - 1]]
          where
            fUpdateInner :: Seq2d Char -> (Int, Int) -> Seq2d Char
            fUpdateInner m (i, j) = update2d i j newVal m
              where
                newVal
                  | oldVal /= '.' = oldVal
                  | isAdjacent 'L' = 'L'
                  | isAdjacent 'R' = 'R'
                  | otherwise = oldVal
                  where
                    oldVal = index2d i j m
                    isAdjacent c = any (\(di, dj) -> in2d (i + di) (j + dj) m && index2d (i + di) (j + dj) m == c) allDeltasCCW

    -- find if 'L' or 'R' is "inside"
    innerChar = if 'L' `elem` boundary then 'R' else 'L'
      where
        boundary =
          [index2d i j filledLrTileMap | i <- [0 .. nRows - 1], j <- [0, nCols - 1]]
            ++ [index2d i j filledLrTileMap | i <- [0, nRows - 1], j <- [0 .. nCols - 1]]
