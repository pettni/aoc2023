module Day4 (solve1, solve2) where

import Data.Char (digitToInt, isDigit)
import qualified Data.IntSet as IS (IntSet, fromList, intersection, size)
import Data.List.Split (splitOn)
import Data.Text (Text)
import qualified Data.Text as T (lines, unpack)

-- Types

data Card = Card {num :: Int, mine :: IS.IntSet, winning :: IS.IntSet} deriving (Show)

-- Part 1

solve1 :: Text -> Int
solve1 ss = sum $ cardScore <$> cards
  where
    cards = readCard . T.unpack <$> T.lines ss
    cardScore :: Card -> Int
    cardScore card = if numMatches == 0 then 0 else 2 ^ (numMatches - 1)
      where
        numMatches = calcNumMatches card

parseNum :: String -> Int
parseNum s = read (filter isDigit s) :: Int

readCard :: String -> Card
readCard line = Card i mine winning
  where
    [part1, part2] = splitOn ": " line
    [part2a, part2b] = splitOn " | " part2
    i = parseNum part1
    mine = IS.fromList $ parseNum <$> filter (not . null) (splitOn " " part2a)
    winning = IS.fromList $ parseNum <$> filter (not . null) (splitOn " " part2b)

calcNumMatches (Card _ winning mine) = IS.size $ winning `IS.intersection` mine

-- Part 2

solve2 :: Text -> Int
solve2 ss = sum numCards
  where
    cards = readCard . T.unpack <$> T.lines ss
    scores = calcNumMatches <$> cards

    numCards = calcNumCards $ zip numCardsInit scores
      where
        numCardsInit = replicate (length cards) 1
        calcNumCards :: [(Int, Int)] -> [Int]
        calcNumCards [] = []
        calcNumCards ((n, score) : xs) = n : calcNumCards xs'
          where
            (ns, ss) = unzip xs
            xs' = zip (zipLongWith (+) ns (replicate score n)) ss

zipLongWith :: (a -> a -> a) -> [a] -> [a] -> [a]
zipLongWith f (x : xs) (y : ys) = f x y : zipLongWith f xs ys
zipLongWith f [] ys = ys
zipLongWith f xs [] = xs
