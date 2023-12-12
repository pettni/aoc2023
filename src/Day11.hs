module Day11 (solve1, solve2, solve2') where

import Data.List (transpose)
import qualified Data.Set as S
import Data.Text (Text, unpack)
import qualified Text.Parsec as Parsec

-- Types

newtype Pos = Pos (Int, Int) deriving (Eq, Show, Ord)

-- Parsing

parseInput :: Text -> [[Char]]
parseInput ss = handleErr $ Parsec.parse inputData "Failure" $ unpack ss
  where
    inputData = Parsec.sepEndBy1 line Parsec.endOfLine
    line = Parsec.many1 (Parsec.noneOf "\n")

    handleErr (Left a) = error (show a)
    handleErr (Right a) = a

-- Part 1

solve1 :: Text -> Int
solve1 ss = sum $ uncurry manhattanDistance <$> [(p0, p1) | p0 <- galaxies, p1 <- galaxies, p0 < p1]
  where
    image = expandImage $ parseInput ss
    galaxies = getGalaxies image

expandImage :: [[Char]] -> [[Char]]
expandImage = transpose . expandRows . transpose . expandRows
  where
    expandRows [] = []
    expandRows (x : xs)
      | all (== '.') x = x : x : expandRows xs
      | otherwise = x : expandRows xs

getGalaxies :: [[Char]] -> [Pos]
getGalaxies image = [Pos (i, j) | i <- [0 .. length image - 1], j <- [0 .. length (image !! i) - 1], image !! i !! j == '#']

manhattanDistance :: Pos -> Pos -> Int
manhattanDistance (Pos (i0, j0)) (Pos (i1, j1)) = abs (i0 - i1) + abs (j0 - j1)

-- Part 2

solve2' :: Int -> Text -> Int
solve2' expFactor ss = sum $ uncurry (manhattanDistance' image expFactor) <$> [(p0, p1) | p0 <- galaxies, p1 <- galaxies, p0 < p1]
  where
    image = parseInput ss
    galaxies = getGalaxies image

solve2 = solve2' 1000000

manhattanDistance' :: [[Char]] -> Int -> Pos -> Pos -> Int
manhattanDistance' image expFactor (Pos (i0, j0)) (Pos (i1, j1)) = (expFactor - 1) * crossedExpansions + abs (i0 - i1) + abs (j0 - j1)
  where
    expandedRows = S.fromList [i | i <- [0 .. length image - 1], all (== '.') $ image !! i]
    expandedCols = S.fromList [j | j <- [0 .. length tImage - 1], all (== '.') $ tImage !! j]
      where
        tImage = transpose image
    crossedExpandedRows = length $ filter (`S.member` expandedRows) [min i0 i1 .. max i0 i1]
    crossedExpandedCols = length $ filter (`S.member` expandedCols) [min j0 j1 .. max j0 j1]
    crossedExpansions = crossedExpandedCols + crossedExpandedRows
