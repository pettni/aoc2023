module Day21 (solve1, solve1', solve2, reachableInfinite) where

import qualified Data.Set as S
import Data.Text (Text, unpack)
import qualified Data.Vector as V
import Debug.Trace (trace)
import qualified Text.Parsec as Parsec

-- Types

data Tile = TRock | TGarden | TStart deriving (Eq, Show)

type Board = V.Vector (V.Vector Tile)

-- Parsing

parseTile :: Char -> Tile
parseTile '.' = TGarden
parseTile '#' = TRock
parseTile 'S' = TStart
parseTile c = error $ "Can not parse " ++ [c]

parseInput :: Text -> Board
parseInput ss = handleErr $ Parsec.parse inputData "Failure" $ unpack ss
  where
    inputData = V.fromList <$> Parsec.sepEndBy1 (V.fromList . map parseTile <$> Parsec.many1 (Parsec.noneOf "\n")) Parsec.endOfLine

    handleErr (Left a) = error (show a)
    handleErr (Right a) = a

-- Part 1

step :: Board -> (Int, Int) -> S.Set (Int, Int)
step board (x, y) = S.fromList $ filter notRock $ filter inBounds [(x + dx, y + dy) | (dx, dy) <- [(-1, 0), (1, 0), (0, -1), (0, 1)]]
  where
    inBounds (x, y) = y >= 0 && y < length board && x >= 0 && x < length (board V.! y)
    notRock (x, y) = board V.! y V.! x /= TRock

solve1' :: Int -> Text -> Int
solve1' numSteps ss = length $ iterate (S.unions . S.map (step board)) startSet !! numSteps
  where
    startSet = S.fromList [(i, j) | j <- [0 .. length board - 1], i <- [0 .. length (board V.! j) - 1], board V.! j V.! i == TStart]
    board = parseInput ss

solve1 :: Text -> Int
solve1 = solve1' 64

-- Part 2

step' :: Board -> (Int, Int) -> S.Set (Int, Int)
step' board (x, y) = S.fromList $ filter notRock [(x + dx, y + dy) | (dx, dy) <- [(-1, 0), (1, 0), (0, -1), (0, 1)]]
  where
    notRock (x, y) = board V.! ym V.! xm /= TRock
      where
        ym = y `mod` length board
        xm = x `mod` length (board V.! ym)

reachableInfinite :: [Int] -> Text -> [Int]
reachableInfinite numSteps ss = length <$> fmap (sequence !!) numSteps
  where
    board = parseInput ss
    startSet = S.fromList [(i, j) | j <- [0 .. length board - 1], i <- [0 .. length (board V.! j) - 1], board V.! j V.! i == TStart]
    sequence = iterate (S.unions . S.map (step' board)) startSet

-- The steps are s.t. 26501365 = 65 + 202300 * 131
-- n = 0 -- expect 3762
-- n = 1 -- expect 33547
-- n = 2 -- expect 93052
-- n = 3 -- expect 182277
solve2 :: Text -> Int
solve2 ss = trace debugStr f 202300
  where
    debugStr = "f0=" ++ show (f 0) ++ " f1=" ++ show (f 1) ++ " f2=" ++ show (f 2) ++ " f3=" ++ show (f 3)
    -- Let A0 be the center diamond, and A1 the offset diamond. We calculate
    -- how many tiles that are reachable in each diamond type in even and odd
    -- configurations. The board side is 131, meaning that it takes 65 steps to reach
    -- the edge of the inner diamond, and 131 additional steps for each board.

    -- ODD:  'S' visible, reaches edge
    -- EVEN: 'S' not visible, does not reach edge

    -- numSteps   Diamond count   Diamond increment
    -- 64      -> 1x A0-even
    -- 65      -> 1x A0-odd       -1 * A0even + 1 * A0odd + 0 * A1
    -- 65+131  -> 1x A0-even
    --            4x A0-odd
    --            2x A1-even
    --            2x A1-odd       1 * A0even + 3 * A0odd + 2 * A1
    -- 65+2*131-> 1x A0-odd
    --            4x A0-even
    --            2x A1-even
    --            2x A1-odd
    --            8x A0-odd
    --            4x A1-even
    --            4x A1-odd       3 * A0even + 5 * A0odd + 4 * A1
    [a0Even, a0Odd, nineAreas] = reachableInfinite [64, 65, 65 + 131] ss
    a1EvenPlusa1Odd = (nineAreas - a0Even - 4 * a0Odd) `div` 2

    -- Increment in each area type is linear in n:
    --   f(n) - f(n-1) = (2n-1) A0even + (2n+1) A0odd + 2n * A1
    -- Sum up ->
    --   f(n) = f(0) + (n * (n+1) - n) A0Even + (n * (n+1) + n) A0Odd + n * (n+1) A1
    f n' =
      a0Odd
        + n' * n' * a0Even
        + n' * (n' + 2) * a0Odd
        + n' * (n' + 1) * a1EvenPlusa1Odd
