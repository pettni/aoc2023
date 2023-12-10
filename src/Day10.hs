module Day10 (solve1, solve2) where

import Control.Exception (throw)
import Data.List (find)
import Data.List.Extra ((!?))
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust, isJust, mapMaybe)
import qualified Data.Sequence (lookup)
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import Debug.Trace (trace)
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

dynamics :: [[Char]] -> State -> Maybe State
dynamics _ Send = Nothing
dynamics t (State ((i0, j0), o0)) = if symbol == Just 'S' then Just Send else getNext (symbol >>= nextOr o0) (i1, j1)
  where
    (i1, j1) = case o0 of
      ON -> (i0 - 1, j0)
      OS -> (i0 + 1, j0)
      OE -> (i0, j0 + 1)
      OW -> (i0, j0 - 1)

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

solve2 :: Text -> Int
solve2 ss = 0
