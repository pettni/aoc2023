module Day13 (solve1, solve2) where

import Data.List (find, transpose)
import Data.Maybe (fromJust, isJust)
import Data.Text (Text, unpack)
import qualified Text.Parsec as Parsec

-- Types

newtype Pattern = Pattern [[Char]] deriving (Show, Eq)

-- Parsing

parseInput :: Text -> [Pattern]
parseInput ss = handleErr $ Parsec.parse inputData "Failure" $ unpack ss
  where
    inputData = Parsec.sepEndBy1 pattern Parsec.endOfLine
    pattern = Pattern <$> Parsec.endBy1 (Parsec.many1 (Parsec.noneOf ['\n'])) Parsec.endOfLine

    handleErr (Left a) = error (show a)
    handleErr (Right a) = a

-- Part 1

solve1 :: Text -> Int
solve1 ss = sum $ reflectionScore (vReflection 0) <$> patterns
  where
    patterns = parseInput ss

vReflection :: Int -> Pattern -> Maybe Int
vReflection numDiff (Pattern p) = find isReflection [1 .. length p - 1]
  where
    isReflection :: Int -> Bool
    isReflection i = numDiff == sum (zipWith countDiff (reverse . take i $ p) (drop i p))
      where
        countDiff :: [Char] -> [Char] -> Int
        countDiff xs ys = length $ filter not $ zipWith (==) xs ys

reflectionScore :: (Pattern -> Maybe Int) -> Pattern -> Int
reflectionScore fVref p@(Pattern cs)
  | isJust vref = 100 * fromJust vref
  | isJust href = fromJust href
  | otherwise = error $ "No reflection found in\n" ++ show p
  where
    vref = fVref p
    href = fVref (Pattern (transpose cs))

-- Part 2

solve2 :: Text -> Int
solve2 ss = sum $ reflectionScore (vReflection 1) <$> patterns
  where
    patterns = parseInput ss
