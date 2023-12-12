module Day12 (solve1, solve2) where

import Data.List (intercalate, transpose)
import Data.Text (Text, unpack)
import Debug.Trace (trace)
import qualified Text.Parsec as Parsec

-- Types

data Condition = COk | CBroken | CUnknown deriving (Show, Eq)

data Record = Record {conditions :: [Condition], brokenGroups :: [Int]} deriving (Show, Eq)

-- Parsing

parseCondition :: Char -> Condition
parseCondition '?' = CUnknown
parseCondition '.' = COk
parseCondition '#' = CBroken
parseCondition c = error $ "Unknown conditon " ++ [c]

parseInput :: Text -> [Record]
parseInput ss = handleErr $ Parsec.parse inputData "Failure" $ unpack ss
  where
    inputData = Parsec.sepEndBy1 record Parsec.endOfLine
    record = do
      conditions <- Parsec.many1 (Parsec.noneOf " ")
      Parsec.spaces
      groups <- Parsec.sepBy1 (Parsec.many1 Parsec.digit) (Parsec.char ',')
      return $ Record (parseCondition <$> conditions) ((\x -> read x :: Int) <$> groups)

    handleErr (Left a) = error (show a)
    handleErr (Right a) = a

-- Part 1

solve1 :: Text -> Int
solve1 ss = sum $ countCombinations <$> records
  where
    records = parseInput ss

countCombinations :: Record -> Int
countCombinations = countCombinations' CUnknown
  where
    countCombinations' :: Condition -> Record -> Int
    -- consumed all input
    countCombinations' _ (Record [] []) = 1
    -- need more broken
    countCombinations' _ (Record [] gs) = 0
    -- need more groups
    countCombinations' _ (Record (CBroken : cs) []) = 0
    -- need broken
    countCombinations' CBroken (Record (COk : cs) _) = 0
    -- need ok
    countCombinations' COk (Record (CBroken : cs) _) = 0
    -- skip over OK
    countCombinations' _ (Record (COk : cs) gs) = countCombinations' CUnknown $ Record cs gs
    -- consume broken
    countCombinations' _ (Record (CBroken : cs) (g : gs)) = countCombinations' nextCond $ Record cs newGroups
      where
        (newGroups, nextCond) = if g == 1 then (gs, COk) else (g - 1 : gs, CBroken)
    -- disambiguate unknown
    countCombinations' flag (Record (CUnknown : cs) gs) = combsOk + combsBroken
      where
        combsOk = countCombinations' flag $ Record (COk : cs) gs
        combsBroken = countCombinations' flag $ Record (CBroken : cs) gs
    -- pattern match fail -> invalid combination
    countCombinations' cond rec = error $ "Invalid comb: " ++ show cond ++ " (rec :" ++ show rec ++ ")"

-- Part 2

solve2 :: Text -> Int
solve2 ss = sum $ countCombinations <$> records
  where
    records = repeatRecord <$> parseInput ss

repeatRecord :: Record -> Record
repeatRecord (Record cs gs) = Record (intercalate [CUnknown] $ replicate 5 cs) (concat $ replicate 5 gs)
