module Day21 (solve1, solve1', solve2, reachableInfinite) where

import qualified Data.Array.Unboxed as A
import Data.Ix (range)
import qualified Data.Set as S
import Data.Text (Text, unpack)
import Debug.Trace (trace)
import qualified Text.Parsec as Parsec

-- Types

data Tile = TRock | TGarden | TStart deriving (Eq, Show)

type Board = A.Array (Int, Int) Tile

-- Parsing

parseTile :: Char -> Tile
parseTile '.' = TGarden
parseTile '#' = TRock
parseTile 'S' = TStart

parseInput :: Text -> Board
parseInput ss = toBoard $ handleErr $ Parsec.parse inputData "Failure" $ unpack ss
  where
    inputData = Parsec.sepEndBy1 (map parseTile <$> Parsec.many1 (Parsec.noneOf "\n")) Parsec.endOfLine

    toBoard :: [[Tile]] -> Board
    toBoard xs = A.listArray ((0, 0), (length xs - 1, length (head xs) - 1)) (concat xs)

    handleErr (Left a) = error (show a)
    handleErr (Right a) = a

-- Part 1

movements :: [(Int, Int) -> (Int, Int)]
movements = [\(x, y) -> (x + dx, y + dy) | (dx, dy) <- [(0, -1), (0, 1), (-1, 0), (1, 0)]]

reachableInfinite :: [Int] -> Text -> [Int]
reachableInfinite numSteps ss = length . (reachableSets !!) <$> numSteps
  where
    board = parseInput ss
    (_, (yMax, xMax)) = A.bounds board
    startSet = S.fromList $ filter ((== TStart) . (board A.!)) $ range (A.bounds board)
    reachableSets = iterate (S.fromList . filter notRock . (movements <*>) . S.toList) startSet
      where
        notRock (y, x) = board A.! (y `mod` (yMax + 1), x `mod` (xMax + 1)) /= TRock

solve1' :: Int -> Text -> Int
solve1' numSteps ss = head $ reachableInfinite [numSteps] ss

solve1 :: Text -> Int
solve1 = solve1' 64

-- Part 2

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
