module Day3 (solve1, solve2) where

import Data.Char (digitToInt, isDigit)
import qualified Data.Map as M (Map, fromList, lookup, toList, (!))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T (lines, unpack)

-- Types

type Cell = (Pos, Tile)

type Schematic = [[Cell]]

type SchematicMap = M.Map Pos Tile

newtype Pos = Pos (Int, Int) deriving (Ord, Eq, Show)

data Tile = TEmpty | TSymbol Char | TNumber Int deriving (Show)

data NumberGroup = NumberGroup {pos :: Pos, len :: Int, val :: Int} deriving (Eq, Show)

data Part = Part Char [NumberGroup] deriving (Show)

-- Part 1

solve1 :: Text -> Int
solve1 ss = sum $ map val validNumberGroups
  where
    sch = buildSchematic ss
    schemaMap = M.fromList $ concat sch
    numberGroups = getNumberGroups sch
    validNumberGroups = filter (validNumber schemaMap) numberGroups

buildSchematic :: Text -> Schematic
buildSchematic ss = [[(Pos (i, j), getTile $ row !! j) | j <- [0 .. length row - 1]] | i <- [0 .. length rows - 1], let row = rows !! i]
  where
    rows = map T.unpack $ T.lines ss
    getTile '.' = TEmpty
    getTile c
      | isDigit c = TNumber . digitToInt $ c
      | otherwise = TSymbol c

getNumberGroups :: Schematic -> [NumberGroup]
getNumberGroups sch = toNumberGroup <$> concatMap groupDigits sch
  where
    groupDigits :: [Cell] -> [[Cell]]
    groupDigits [] = []
    groupDigits xs = if not (null maybe_next_group) then maybe_next_group : rec_tail else rec_tail
      where
        xs_nondigit = dropWhile (not . isDigit') xs
        maybe_next_group = takeWhile isDigit' xs_nondigit
        rec_tail = groupDigits (dropWhile isDigit' xs_nondigit)

        isDigit' (_, TNumber _) = True
        isDigit' _ = False

    toNumberGroup :: [Cell] -> NumberGroup
    toNumberGroup xs = NumberGroup (fst . head $ xs) (length xs) (getValue (map snd xs))
      where
        getValue :: [Tile] -> Int
        getValue xs = sum [v * 10 ^ (length xs - 1 - i) | i <- [0 .. length xs - 1], let v = getNumber (xs !! i)]
          where
            getNumber (TNumber n) = n
            getNumber _ = error "Invalid"

validNumber :: SchematicMap -> NumberGroup -> Bool
validNumber m g = any filterFn allAdjacentTiles
  where
    filterFn (TSymbol n) = True
    filterFn _ = False

    allAdjacentTiles = concatMap (mapMaybe (`M.lookup` m) . adjacent) [Pos (x0, y0 + i) | i <- [0 .. len g - 1]]
      where
        Pos (x0, y0) = pos g

adjacent :: Pos -> [Pos]
adjacent (Pos (x, y)) =
  [ Pos (x - 1, y - 1),
    Pos (x + 0, y - 1),
    Pos (x + 1, y - 1),
    Pos (x + 1, y + 0),
    Pos (x + 1, y + 1),
    Pos (x + 0, y + 1),
    Pos (x - 1, y + 1),
    Pos (x - 1, y + 0)
  ]

-- Part 2

solve2 :: Text -> Int
solve2 ss = sum $ mapMaybe getGearRatio parts
  where
    sch = buildSchematic ss
    schemaMap = M.fromList $ concat sch
    numberGroups = getNumberGroups sch
    parts = getParts schemaMap numberGroups

getGearRatio :: Part -> Maybe Int
getGearRatio (Part '*' [n1, n2]) = Just (val n1 * val n2)
getGearRatio _ = Nothing

getParts :: SchematicMap -> [NumberGroup] -> [Part]
getParts schemaMap numberGroups = map makeParts symbolLocs
  where
    symbolLocs = mapMaybe mapFn (M.toList schemaMap)
      where
        mapFn (pos, TSymbol c) = Just (pos, c)
        mapFn _ = Nothing
    makeParts :: (Pos, Char) -> Part
    makeParts (pos, c) = Part c (getAdjacentNumbers schemaMap numberGroups pos)

isAdjacent :: SchematicMap -> Pos -> NumberGroup -> Bool
isAdjacent m p g = p `elem` allAdjacentPos
  where
    allAdjacentPos = concatMap adjacent [Pos (x0, y0 + i) | i <- [0 .. len g - 1]]
      where
        Pos (x0, y0) = pos g

getAdjacentNumbers :: SchematicMap -> [NumberGroup] -> Pos -> [NumberGroup]
getAdjacentNumbers schemaMap numberGroups pos = filter (isAdjacent schemaMap pos) numberGroups
