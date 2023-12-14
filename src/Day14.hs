module Day14 (solve1, solve2) where

import Data.List (intercalate, transpose)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Text (Text, unpack)
import qualified Text.Parsec as Parsec

-- Types

data Rock = RRound | RSquare | RNone deriving (Eq, Ord, Show)

-- Parsing

parseRock :: Char -> Rock
parseRock 'O' = RRound
parseRock '#' = RSquare
parseRock '.' = RNone

parseInput :: Text -> [[Rock]]
parseInput ss = handleErr $ Parsec.parse inputData "Failure" $ unpack ss
  where
    inputData = Parsec.sepEndBy1 inputLine Parsec.endOfLine
    inputLine = fmap parseRock <$> Parsec.many1 (Parsec.noneOf "\n")

    handleErr (Left a) = error (show a)
    handleErr (Right a) = a

-- Part 1

solve1 :: Text -> Int
solve1 ss = sum $ computeWeight <$> transpose (parseInput ss)

computeWeight :: [Rock] -> Int
computeWeight rocks = snd $ foldl fnFold (0, 0) $ zip [1 ..] (reverse rocks ++ [RSquare])
  where
    fnFold :: (Int, Int) -> (Int, Rock) -> (Int, Int)
    fnFold (count, score) (pos, RNone) = (count, score)
    fnFold (count, score) (pos, RRound) = (count + 1, score)
    fnFold (count, score) (pos, RSquare) = (0, score + sum [pos - count .. pos - 1])

-- Part 2

solve2 :: Text -> Int
solve2 ss = sum $ sum . zipWith scoreRock [1 ..] . reverse <$> transpose k1
  where
    scoreRock :: Int -> Rock -> Int
    scoreRock val RRound = val
    scoreRock _ _ = 0

    (cycleStart, cyclePeriod, boardAtCycleStart) = findCycle $ parseInput ss
    numAdditionalCycles = (1000000000 - cycleStart) `mod` cyclePeriod
    k1 = foldl (\a i -> doCycle a) boardAtCycleStart [1 .. numAdditionalCycles]

findCycle :: [[Rock]] -> (Int, Int, [[Rock]])
findCycle rocks = (cycleStart, cyclePeriod, boardFinal)
  where
    cycleStart = fromJust $ M.lookup boardFinal memFinal
    cyclePeriod = idxFinal - cycleStart
    (boardFinal, memFinal, idxFinal) = head $ dropWhile (not . inMem) $ scanl fnFold (rocks, mempty, 0) [1 ..]
      where
        inMem :: ([[Rock]], M.Map [[Rock]] Int, Int) -> Bool
        inMem (board, mem, _) = M.member board mem

        fnFold :: ([[Rock]], M.Map [[Rock]] Int, Int) -> Int -> ([[Rock]], M.Map [[Rock]] Int, Int)
        fnFold (boardPre, mem, _) idx = (doCycle boardPre, M.insert boardPre (idx - 1) mem, idx)

pushRocks :: Bool -> [Rock] -> [Rock]
pushRocks reverse rocks = intercalate [RSquare] $ reorg <$> splitOn [RSquare] rocks
  where
    reorg :: [Rock] -> [Rock]
    reorg rs = if reverse then rround ++ rnone else rnone ++ rround
      where
        rnone = replicate (length $ filter (== RNone) rs) RNone
        rround = replicate (length $ filter (== RRound) rs) RRound

pushEast :: [[Rock]] -> [[Rock]]
pushEast rocks = pushRocks False <$> rocks

pushSouth :: [[Rock]] -> [[Rock]]
pushSouth = transpose . pushEast . transpose

pushWest :: [[Rock]] -> [[Rock]]
pushWest rocks = pushRocks True <$> rocks

pushNorth :: [[Rock]] -> [[Rock]]
pushNorth = transpose . pushWest . transpose

doCycle :: [[Rock]] -> [[Rock]]
doCycle = pushEast . pushSouth . pushWest . pushNorth
