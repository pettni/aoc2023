module Day12 (solve1, solve2) where

import Data.List (intercalate, transpose)
import qualified Data.Map as M (Map, fromList, insert, lookup, member, union)
import Data.Maybe (fromJust)
import Data.Text (Text, unpack)
import Debug.Trace (trace)
import qualified Text.Parsec as Parsec

-- Types

data Condition = COk | CBroken | CUnknown deriving (Show, Eq, Ord)

data Record = Record {conditions :: [Condition], brokenGroups :: [Int]} deriving (Show, Eq, Ord)

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
countCombinations = snd . countCombinations' mempty CUnknown
  where
    countCombinations' :: M.Map (Condition, Record) Int -> Condition -> Record -> (M.Map (Condition, Record) Int, Int)
    -- consumed all input
    countCombinations' mem _ (Record [] []) = (mem, 1)
    -- need more broken
    countCombinations' mem _ (Record [] gs) = (mem, 0)
    -- need more groups
    countCombinations' mem _ (Record (CBroken : cs) []) = (mem, 0)
    -- need broken
    countCombinations' mem CBroken (Record (COk : cs) _) = (mem, 0)
    -- need ok
    countCombinations' mem COk (Record (CBroken : cs) _) = (mem, 0)
    -- skip over OK
    countCombinations' mem _ (Record (COk : cs) gs) = countCombinations' mem CUnknown $ Record cs gs
    -- consume broken
    countCombinations' mem _ (Record (CBroken : cs) (g : gs)) = countCombinations' mem nextCond $ Record cs newGroups
      where
        (newGroups, nextCond) = if g == 1 then (gs, COk) else (g - 1 : gs, CBroken)
    -- disambiguate unknown
    countCombinations' mem flag rec@(Record (CUnknown : cs) gs) = if M.member (flag, rec) mem then (mem, fromJust $ M.lookup (flag, rec) mem) else (memRec, combsRec)
      where
        (memRec, combsRec) = (mem''', combsOk + combsBroken)
          where
            (mem', combsOk) = countCombinations' mem flag $ Record (COk : cs) gs
            (mem'', combsBroken) = countCombinations' mem' flag $ Record (CBroken : cs) gs
            mem''' = M.insert (flag, rec) (combsOk + combsBroken) mem''

-- Part 2

solve2 :: Text -> Int
solve2 ss = sum $ countCombinations <$> records
  where
    records = repeatRecord <$> parseInput ss

repeatRecord :: Record -> Record
repeatRecord (Record cs gs) = Record (intercalate [CUnknown] $ replicate 5 cs) (concat $ replicate 5 gs)
