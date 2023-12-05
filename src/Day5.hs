module Day5 (solve1, solve2) where

import Control.Monad (when)
import Data.Char (isDigit)
import Data.Either (fromRight)
import Data.Functor.Identity (Identity)
import Data.List (find)
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import Data.Tuple.Extra (uncurry3)
import Debug.Trace (trace)
import qualified Text.Parsec as Parsec

-- Types

data SingleRangeMap = SingleRangeMap {dst :: Int, src :: Int, len :: Int} deriving (Show)

type RangeMap = [SingleRangeMap]

data NamedRangeMap = NamedRangeMap {from :: String, to :: String, maps :: RangeMap}

mapValue :: RangeMap -> Int -> Int
mapValue rangeMap x = case find (\singleRangeMap -> x >= src singleRangeMap && x < src singleRangeMap + len singleRangeMap) rangeMap of
  Nothing -> x
  Just m -> dst m + x - src m

-- Part 1

solve1 :: Text -> Int
solve1 ss = minimum locations
  where
    (seeds, rangeMaps) = parseInput ss
    locations = foldl (\x m -> mapValue (maps m) <$> x) seeds rangeMaps

parseInput :: Text -> ([Int], [NamedRangeMap])
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
      seeds <- intLine
      Parsec.endOfLine
      return seeds

    -- "seed-to-soil map:",
    -- "50 98 2",
    -- "52 50 48",  (including eol)
    mapBlock :: Parsec.ParsecT String u Identity NamedRangeMap
    mapBlock = do
      (from, to) <- mapHeader
      Parsec.endOfLine
      intLines <- Parsec.endBy intLine Parsec.endOfLine
      return $ NamedRangeMap from to $ uncurry3 SingleRangeMap . tuplify3 <$> intLines
      where
        tuplify3 [x, y, z] = (x, y, z)

    -- "seed-to-soil map:" (no eol)
    mapHeader :: Parsec.ParsecT String u Identity (String, String)
    mapHeader = do
      from <- Parsec.manyTill Parsec.anyToken (Parsec.string "-to-")
      to <- Parsec.manyTill Parsec.anyToken (Parsec.string " map:")
      return (from, to)

    -- "50 98 2"  (no eol)
    intLine :: Parsec.ParsecT String u Identity [Int]
    intLine = do
      numbers <- Parsec.sepBy1 (Parsec.many1 Parsec.digit) (Parsec.char ' ')
      return (parseNum <$> numbers)

parseNum :: String -> Int
parseNum s = read (filter isDigit s) :: Int

-- Part 2

solve2 :: Text -> Int
solve2 ss = minimum locations
  where
    (seeds_ranges, rangeMaps) = parseInput ss
    seeds = getRanges seeds_ranges
      where
        getRanges :: [Int] -> [Int]
        getRanges [] = []
        getRanges (x : y : xs) = [x .. x + y] ++ getRanges xs

    locations = foldl (\x m -> mapValue (maps m) <$> x) seeds rangeMaps
