module Day8 (solve1, solve2) where

import Data.Either (fromRight)
import Data.List (takeWhile)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import Debug.Trace (trace)
import qualified Text.Parsec as Parsec

-- Types

data Dir = DL | DR deriving (Show, Eq)

parseDir :: Char -> Dir
parseDir 'R' = DR
parseDir 'L' = DL

-- Parsing

parseInput :: Text -> ([Dir], M.Map String (String, String))
parseInput ss = fromRight (error "Failed to parse") $ Parsec.parse inputData "Failure" $ T.unpack ss
  where
    inputData = do
      dirs <- Parsec.manyTill Parsec.anyChar Parsec.endOfLine
      Parsec.endOfLine
      lines <- Parsec.endBy mapEdge Parsec.endOfLine
      Parsec.eof
      return (parseDir <$> dirs, M.fromList lines)

    mapEdge = do
      src <- nodeName
      Parsec.string " = "
      [left, rght] <- Parsec.between (Parsec.char '(') (Parsec.char ')') (Parsec.sepBy nodeName (Parsec.string ", "))
      return (src, (left, rght))

    nodeName = Parsec.count 3 Parsec.anyChar

-- Part 1

trajectory :: (a -> s -> s) -> [a] -> s -> [s]
trajectory t (a : as) s0 = s0 : trajectory t as (t a s0)

solve1 :: Text -> Int
solve1 ss = length $ takeWhile (/= "ZZZ") (trajectory (dynX map) (cycle dirs) "AAA")
  where
    (dirs, map) = parseInput ss

-- dynamics with state (x)
dynX :: M.Map String (String, String) -> Dir -> String -> String
dynX map a x = (if a == DL then fst else snd) (fromJust (M.lookup x map))

-- Part 2

-- (D, P) s.t. for a Markov sequence a[D + n * P] = a[D + P] for all n, and D is minimal.
periodicity :: (Ord a) => [a] -> (Int, Int)
periodicity = periodicity' M.empty 0
  where
    periodicity' :: (Ord a) => M.Map a Int -> Int -> [a] -> (Int, Int)
    periodicity' seen cnt (a : as)
      | M.member a seen = (cnt_first, cnt - cnt_first)
      | otherwise = periodicity' (M.insert a cnt seen) (cnt + 1) as
      where
        cnt_first = fromJust $ M.lookup a seen

-- (prefix length, end indices in prefix, cycle length, end indices in suffix)
getPeriodicityIndices :: (Ord a) => (a -> Bool) -> [a] -> (Int, [Int], Int, [Int])
getPeriodicityIndices pred xs = (nPref, idxInPref, nCycle, idxInCycle)
  where
    (nPref, nCycle) = periodicity xs
    idxInPref = fst <$> (filter (pred . snd) . zip [0 ..] . take nPref $ xs)
    idxInCycle = fst <$> (filter (pred . snd) . zip [0 ..] . take nCycle . drop nPref $ xs)

firstCommon2 :: (Ord a) => [a] -> [a] -> a
firstCommon2 (x : xs) (y : ys)
  | x == y = x
  | x < y = firstCommon2 xs (y : ys)
  | x > y = firstCommon2 (x : xs) ys

firstCommonN :: (Ord a) => [[a]] -> a
firstCommonN [a, b] = firstCommon2 a b
firstCommonN ((a : as) : bs)
  | a == firstCommonN bs = a
  | otherwise = firstCommonN (as : bs)

solve2 :: Text -> Int
solve2 ss = if all isSimple periodicities then simpleSolution else difficultSolution
  where
    (dirs, transitionMap) = parseInput ss
    periodicities = getPeriodicityIndices ((==) 'Z' . last . snd) . getTrajectory <$> startNodes
      where
        startNodes = filter ((== 'A') . last) $ M.keys transitionMap
        -- dynamics with state (a, x)
        dynAX :: Dir -> (Int, String) -> (Int, String)
        dynAX a (n, x) = ((n + 1) `mod` length dirs, dynX transitionMap a x)
        getTrajectory x0 = trajectory dynAX (cycle dirs) (0, x0)

    -- problem is s.t. we can easily solve with lcm
    isSimple (nPref, [], nCycle, [c0]) = nPref + c0 == nCycle
    isSimple _ = False

    simpleSolution = foldl lcm 1 $ map (\(_, _, c, _) -> c) periodicities

    -- brute-force-like solution via iterators over all end indices and finding common index
    difficultSolution = firstCommonN $ getStopIndices <$> periodicities
      where
        getStopIndices (nPref, idxInPref, nCycle, idxInCycle) = idxInPref ++ [nPref + c + n * nCycle | n <- [0 ..], c <- idxInCycle]
