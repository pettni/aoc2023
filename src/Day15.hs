module Day15 (solve1, solve2) where

import Data.Char (ord)
import Data.List (find)
import Data.Maybe (isJust)
import Data.Text (Text, unpack)
import qualified Text.Parsec as Parsec

-- Types

data Operation = OMinus | OEqual Int deriving (Eq, Show)

data Step = Step {label :: [Char], op :: Operation} deriving (Eq, Show)

data Lens = Lens {lab :: String, focal :: Int} deriving (Eq, Show)

-- Parsing

parseInput cellType ss = handleErr $ Parsec.parse inputData "Failure" $ unpack ss
  where
    inputData = Parsec.sepBy1 cellType (Parsec.char ',')

    handleErr (Left a) = error (show a)
    handleErr (Right a) = a

cell1 = Parsec.many1 (Parsec.noneOf ",\n")

cell2 = do
  label <- Parsec.many1 (Parsec.noneOf ",\n-=")
  op <- Parsec.many1 (Parsec.noneOf ",\n")
  return (Step label (parseOp op))

parseOp :: [Char] -> Operation
parseOp "-" = OMinus
parseOp ('=' : os) = OEqual (read os :: Int)
parseOp o = error $ "Unknown operation " ++ o

-- Part 1

solve1 :: Text -> Int
solve1 ss = sum $ fnHash <$> parseInput cell1 ss

fnHash :: [Char] -> Int
fnHash = foldl (\curVal c -> (17 * (curVal + ord c)) `mod` 256) 0

-- Part 2

solve2 :: Text -> Int
solve2 ss = computeScore boxesFinal
  where
    steps = parseInput cell2 ss
    boxesFinal = foldl updateBoxes [[] | _ <- [1 .. 256]] steps

updateBoxes :: [[Lens]] -> Step -> [[Lens]]
updateBoxes boxesPre (Step stepLabel op) = take boxNr boxesPre ++ [activePost] ++ drop (boxNr + 1) boxesPre
  where
    boxNr = fnHash stepLabel
    activePre = boxesPre !! boxNr
    activePost = case op of
      OMinus -> filter (\(Lens lensLabel _) -> lensLabel /= stepLabel) activePre
      OEqual n ->
        if inBox
          then map (\(Lens lensLabel f) -> Lens lensLabel (if lensLabel == stepLabel then n else f)) activePre
          else Lens stepLabel n : activePre
        where
          inBox = isJust $ find (\(Lens l _) -> l == stepLabel) activePre

computeScore :: [[Lens]] -> Int
computeScore boxes = foldl (\score (i, box) -> score + i * boxScore box) 0 $ zip [1 ..] boxes
  where
    boxScore box = foldl (\score (slot_idx, Lens _ f) -> score + slot_idx * f) 0 $ zip [1 ..] (reverse box)
