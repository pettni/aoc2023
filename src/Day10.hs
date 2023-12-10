module Day10 (solve1, solve2) where

import Control.Exception (throw)
import Data.List (find)
import qualified Data.Map as M
import Data.Maybe (fromJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import Debug.Trace (trace)
import qualified Text.Parsec as Parsec

-- Types

data Orientation = ON | OS | OW | OE deriving (Show, Eq)

data State = State ((Int, Int), Orientation) | SS deriving (Show, Eq)

-- Parsing

parseInput :: Text -> [[Char]]
parseInput ss = handleErr $ Parsec.parse inputData "Failure" $ T.unpack ss
  where
    inputData = Parsec.sepEndBy1 line Parsec.endOfLine
    line = Parsec.many1 (Parsec.noneOf "\n")

    handleErr (Left a) = error (show a)
    handleErr (Right a) = a

-- Part 1

transition :: Orientation -> Char -> Maybe Orientation
transition ON '|' = Just ON
transition OS '|' = Just OS
transition OW '-' = Just OW
transition OE '-' = Just OE
transition OS 'L' = Just OE
transition OW 'L' = Just ON
transition OS 'J' = Just OW
transition OE 'J' = Just ON
transition OE '7' = Just OS
transition ON '7' = Just OW
transition ON 'F' = Just OE
transition OW 'F' = Just OS
transition _ _ = Nothing

getDynamics :: M.Map (Int, Int) Char -> State -> Maybe State
getDynamics t (State ((i0, j0), o0)) = if symbol == Just 'S' then Just SS else getNewState mNewOr newPos
  where
    newPos = case o0 of
      ON -> (i0 - 1, j0)
      OS -> (i0 + 1, j0)
      OE -> (i0, j0 + 1)
      OW -> (i0, j0 - 1)

    symbol = M.lookup newPos t

    mNewOr = symbol >>= transition o0

    getNewState :: Maybe Orientation -> (Int, Int) -> Maybe State
    getNewState (Just newOr) newPos = Just (State (newPos, newOr))
    getNewState _ _ = Nothing

createTrajectory :: (State -> Maybe State) -> Maybe State -> [Maybe State]
createTrajectory t s0 = s0 : createTrajectory t (s0 >>= t)

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive f (x : xs) = x : if f x then takeWhileInclusive f xs else []

-- newState ::  Maybe Orientation

solve1 :: Text -> Int
solve1 ss = (`div` 2) . (\x -> x - 1) . length . head $ loops
  where
    input = parseInput ss

    tilesWithIdx = [((i, j), c) | (i, js) <- zip [0 ..] $ zip [0 ..] <$> input, (j, c) <- js]

    startIdx = fst . fromJust $ find ((== 'S') . snd) tilesWithIdx

    dynamics = getDynamics $ M.fromList tilesWithIdx

    -- find loop
    --   end if:
    --    1. come back to 'S'
    --    2. get to invalid state
    --    3. loop back to another visited state (seems like this does not happen)
    traj = createTrajectory dynamics $ Just (State (startIdx, OS))

    -- possible start states
    startStates =
      [ Just (State (startIdx, OS)),
        Just (State (startIdx, ON)),
        Just (State (startIdx, OW)),
        Just (State (startIdx, OE))
      ]

    -- expect exactly two loops (one in each direction)
    loops = mapMaybe getLoop startStates
      where
        getLoop x0
          | last traj == Just SS = Just traj
          | otherwise = Nothing
          where
            traj = takeWhileInclusive fStop $ createTrajectory dynamics x0
              where
                fStop Nothing = False
                fStop (Just SS) = False
                fStop _ = True

-- Part 2

solve2 :: Text -> Int
solve2 ss = 0
