module Day18 (solve1, solve2) where

import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import Data.Text (Text, unpack)
import Debug.Trace (trace)
import qualified Text.Parsec as Parsec

-- Types

data Dir = TU | TR | TD | TL deriving (Eq, Show)

data Instruction = Instruction {dir :: Dir, len :: Int, color :: String} deriving (Eq, Show)

-- Parsing

parseDir :: Char -> Dir
parseDir 'U' = TU
parseDir 'R' = TR
parseDir 'D' = TD
parseDir 'L' = TL

parseInput :: Text -> [Instruction]
parseInput ss = handleErr $ Parsec.parse inputData "Failure" $ unpack ss
  where
    inputData = Parsec.sepEndBy1 instructionLine Parsec.endOfLine
    instructionLine = do
      dir <- parseDir <$> Parsec.anyChar
      Parsec.spaces
      len <- read <$> Parsec.many1 Parsec.digit
      Parsec.spaces
      color <- filter (/= '#') <$> Parsec.between (Parsec.char '(') (Parsec.char ')') (Parsec.many1 (Parsec.noneOf ")"))
      return (Instruction dir len color)
    handleErr (Left a) = error (show a)
    handleErr (Right a) = a

-- Part 1

cross :: (Int, Int) -> (Int, Int) -> Int
cross (x0, y0) (x1, y1) = x0 * y1 - y0 * x1

step :: Dir -> Int -> (Int, Int) -> (Int, Int)
step TU l (x, y) = (x, y - l)
step TL l (x, y) = (x - l, y)
step TR l (x, y) = step TL (-l) (x, y)
step TD l (x, y) = step TU (-l) (x, y)

-- Inner area does not count outer area of boxes we pass through. Need to account for that:
--
-- Pass straight through: add 1/2
-- Inwards turn: add 3/4
-- Outwards turn: add 1/4
--
-- Assuming a loop we know that we make a net of 4 inwards turns, which makes up a total of 1 box area.
-- Can therefore count 1/2 for each box we pass through, and add 1.
getArea :: [Instruction] -> Int
getArea instructions = innerArea + (pathLength `div` 2) + 1
  where
    points = (\x -> take (length x) x) $ scanl fnAdvance (0, 0) instructions
      where
        fnAdvance pos (Instruction dir len _) = step dir len pos
    innerArea = abs $ (`div` 2) $ foldl (\acc (p, p') -> acc + cross p p') 0 $ zip points (drop 1 $ cycle points)
    pathLength = foldl (\cum ((x, y), (x', y')) -> cum + abs (x - x') + abs (y - y')) 0 $ zip points (drop 1 $ cycle points)

solve1 :: Text -> Int
solve1 ss = getArea $ parseInput ss

-- Part 2

solve2 :: Text -> Int
solve2 ss = getArea $ fixInstruction <$> parseInput ss

fixInstruction :: Instruction -> Instruction
fixInstruction (Instruction pos dir color) = Instruction (getDir $ last color) (read $ "0x" ++ take 5 color) ""
  where
    getDir '0' = TR
    getDir '1' = TD
    getDir '2' = TL
    getDir '3' = TU
