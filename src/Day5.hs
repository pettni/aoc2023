module Day5 (solve1, solve2, Interval (..), mapIvals, SingleRangeMap (..)) where

import Data.Char (isDigit)
import Data.Either (fromRight)
import Data.Functor.Identity (Identity)
import Data.List (find)
import Data.List.Extra (sortOn)
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import Data.Tuple.Extra (uncurry3)
import qualified Text.Parsec as Parsec

-- Types

data Interval = Interval {ivalmin :: Int, ivalmax :: Int} deriving (Show, Eq)

data SingleRangeMap = SingleRangeMap {dst :: Int, src :: Int, len :: Int} deriving (Show)

data RangeMap = RangeMap {from :: String, to :: String, maps :: [SingleRangeMap]} deriving (Show)

-- Common

parseInput :: Text -> ([Int], [RangeMap])
parseInput ss = fromRight (error "Failed to parse") $ Parsec.parse mapFile "Failure" $ T.unpack ss
  where
    mapFile = do
      seeds <- seedLine
      Parsec.endOfLine
      blocks <- Parsec.sepBy mapBlock Parsec.endOfLine
      Parsec.eof
      return (seeds, blocks)

    -- "seeds: 79 14 55 13" (including eol)
    seedLine = do
      Parsec.string "seeds: "
      seeds <- intLineBase
      Parsec.endOfLine
      return seeds

    -- "seed-to-soil map:",
    -- "50 98 2",
    -- "52 50 48",  (including eol)
    mapBlock = do
      from <- Parsec.manyTill Parsec.anyToken (Parsec.string "-to-")
      to <- Parsec.manyTill Parsec.anyToken (Parsec.string " map:\n")
      intLines <- Parsec.endBy intLineBase Parsec.endOfLine
      return $ RangeMap from to $ uncurry3 SingleRangeMap . tuplify3 <$> intLines
      where
        tuplify3 [x, y, z] = (x, y, z)

    -- "50 98 2"  (no eol)
    intLineBase = fmap (\x -> read x :: Int) <$> Parsec.sepBy1 (Parsec.many1 Parsec.digit) (Parsec.char ' ')

-- Part 1

mapValue :: [SingleRangeMap] -> Int -> Int
mapValue rangeMap x = case find (\singleRangeMap -> x >= src singleRangeMap && x < src singleRangeMap + len singleRangeMap) rangeMap of
  Nothing -> x
  Just m -> dst m + x - src m

solve1 :: Text -> Int
solve1 ss = minimum locations
  where
    (seeds, rangeMaps) = parseInput ss
    locations = foldl (\x m -> mapValue (maps m) <$> x) seeds rangeMaps

-- Part 2

solve2 :: Text -> Int
solve2 ss = minimum $ ivalmin <$> locations
  where
    (seeds_ranges, rangeMaps) = parseInput ss
    seeds = getIvals seeds_ranges
      where
        getIvals :: [Int] -> [Interval]
        getIvals [] = []
        getIvals (x : y : xs) = Interval {ivalmin = x, ivalmax = x + y - 1} : getIvals xs

    locations = foldl (\x m -> mapIvals (maps m) x) seeds rangeMaps

mapIvals :: [SingleRangeMap] -> [Interval] -> [Interval]
mapIvals rs = ivalSimplify . mapIvalsSorted (sortOn src rs) . ivalSimplify
  where
    mapIvalsSorted :: [SingleRangeMap] -> [Interval] -> [Interval]
    mapIvalsSorted [] is = is -- no mapping, copy
    mapIvalsSorted rs [] = [] -- no intervals, empty
    mapIvalsSorted (r@(SingleRangeMap dst src len) : rs) (i@(Interval i0 i1) : is)
      -- drop range
      | r1 < i0 = mapIvalsSorted rs (i : is)
      -- pass through interval
      | i1 < r0 = i : mapIvalsSorted (r : rs) is
      -- split interval
      | i0 < r0 = mapIvalsSorted (r : rs) (Interval i0 (r0 - 1) : Interval r0 i1 : is)
      | i1 > r1 = mapIvalsSorted (r : rs) (Interval i0 r1 : Interval (r1 + 1) i1 : is)
      -- map interval
      | r0 <= i0 && i1 <= r1 = Interval (dst + i0 - r0) (dst + i1 - r0) : mapIvalsSorted (r : rs) is
      where
        r0 = src
        r1 = src + len - 1

ivalUnion :: Interval -> Interval -> Interval
ivalUnion a b = Interval {ivalmin = min (ivalmin a) (ivalmin b), ivalmax = max (ivalmax a) (ivalmax b)}

ivalSimplify :: [Interval] -> [Interval]
ivalSimplify = merge_overlapping_adjacent . sortOn (\x -> (ivalmin x, -ivalmax x))
  where
    merge_overlapping_adjacent [] = []
    merge_overlapping_adjacent [x] = [x]
    merge_overlapping_adjacent (x : y : xs)
      | canDoUnion = merge_overlapping_adjacent $ (x `ivalUnion` y) : xs
      | otherwise = x : merge_overlapping_adjacent (y : xs)
      where
        canDoUnion = max (ivalmin x) (ivalmin y) <= min (ivalmax x) (ivalmax y) + 1
