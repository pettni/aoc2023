module Day4 (solve1, solve2) where

import Data.Char (digitToInt, isDigit)
import Data.List.Split (splitOn)
import Data.Text (Text)
import qualified Data.Text as T (lines, unpack)
import Debug.Trace (trace)

-- Types

data Card = Card {num :: Int, mine :: [Int], winning :: [Int]} deriving (Show)

-- Part 1

solve1 :: Text -> Int
solve1 ss = sum $ cardScore <$> cards
  where
    cards = readCard . T.unpack <$> T.lines ss

parseNum :: String -> Int
parseNum s = read (filter isDigit s) :: Int

readCard :: String -> Card
readCard line = Card i mine winning
  where
    [part1, part2] = splitOn ": " line
    [part2a, part2b] = splitOn " | " part2
    i = parseNum part1
    mine = parseNum <$> filter (not . null) (splitOn " " part2a)
    winning = parseNum <$> filter (not . null) (splitOn " " part2b)

cardScore :: Card -> Int
cardScore (Card i mine winning) = if numMatches == 0 then 0 else 2 ^ (numMatches - 1)
  where
    numMatches = length [x | x <- winning, x `elem` mine]

-- Part 2

solve2 :: Text -> Int
solve2 ss = sum $ trace (show numCards) numCards
  where
    cards = readCard . T.unpack <$> T.lines ss
    scores = cardScore' <$> cards
    numCardsAcc = foldl fScan [] scores
      where
        fScan :: [Int] -> Int -> [Int]
        fScan prev new = filter (> 0) [x - 1 | x <- prev] ++ [new]
    numCards = length <$> trace (show numCardsAcc) numCardsAcc

cardScore' :: Card -> Int
cardScore' (Card i mine winning) = numMatches
  where
    numMatches = length [x | x <- winning, x `elem` mine]
