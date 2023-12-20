module Day20 (solve1, solve2) where

import Data.List (nub)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text, unpack)
import qualified Text.Parsec as Parsec

-- Types

data ModuleType = TBroadcast | TFlipFlop Bool | TConjuction [Pulse] deriving (Eq, Show)

data Module = Module {getType :: ModuleType, getDests :: [String]} deriving (Eq, Show)

data Pulse = TLow | THigh deriving (Eq, Show)

data PulseTransmission = PulseTransmission {getFrom :: String, getTo :: String, getPulse :: Pulse} deriving (Eq, Show)

-- Parsing

parseInput :: Text -> M.Map String Module
parseInput ss = handleErr $ Parsec.parse inputData "Failure" $ unpack ss
  where
    inputData = M.fromList <$> Parsec.sepEndBy1 mod Parsec.endOfLine

    mod = do
      (modName, modType) <- moduleNameAndType
      Parsec.string " -> "
      dests <- Parsec.sepBy1 (Parsec.many1 (Parsec.noneOf ",\n")) (Parsec.string ", ")
      return (modName, Module modType dests)

    moduleNameAndType = broadcastModule Parsec.<|> flipFlopModule Parsec.<|> conjunctionModule

    broadcastModule = do
      Parsec.string "broadcaster"
      return ("broadcaster", TBroadcast)

    flipFlopModule = do
      Parsec.char '%'
      name <- Parsec.many1 (Parsec.noneOf " ")
      return (name, TFlipFlop False)

    conjunctionModule = do
      Parsec.char '&'
      name <- Parsec.many1 (Parsec.noneOf " ")
      return (name, TConjuction [])

    handleErr (Left a) = error (show a)
    handleErr (Right a) = a

-- Part 1

-- Update conjuction modules by initializing the internal flags to have length equal to the number of inputs,
-- and compute map { toModule : { fromModule : input_idx } } for retrieving the input index of a transition.
prepareSystem :: M.Map String Module -> (M.Map String Module, M.Map String (M.Map String Int))
prepareSystem modMap = (modMapOut, idxMaps)
  where
    edges = [(from, to) | from <- M.keys modMap, let mod = modMap M.! from, to <- getDests mod]

    -- update conjunction modules by intializing flags
    modMapOut = M.mapWithKey fnMap modMap
      where
        fnMap :: String -> Module -> Module
        fnMap mName mIn@(Module (TConjuction _) dests) = Module (TConjuction (replicate (getNumInputsTo mName) TLow)) dests
        fnMap mName mIn = mIn

        getNumInputsTo :: String -> Int
        getNumInputsTo toName = length $ map fst $ filter ((== toName) . snd) edges

    -- compute input index maps
    idxMaps = M.fromList [(name, getInputMap name) | name <- S.toList allTos]
      where
        getInputMap toName = M.fromList $ zip (map fst $ filter ((== toName) . snd) edges) [0 ..]
        allTos = S.fromList $ map snd edges

-- Propagate a single pulse transmission
step :: M.Map String Module -> M.Map String (M.Map String Int) -> PulseTransmission -> (M.Map String Module, [PulseTransmission])
step modMap inputIdxMaps ptIn@(PulseTransmission from to pulse)
  | not $ M.member to modMap = (modMap, [])
  | otherwise = (M.insert to activeModuleOut modMap, outSignals)
  where
    -- update module
    activeModuleOut = case (modMap M.! to, pulse) of
      (m@(Module TBroadcast _), _) -> m
      (m@(Module (TFlipFlop _) _), THigh) -> m
      (Module (TFlipFlop xIn) dests, TLow) -> Module (TFlipFlop (not xIn)) dests
      (Module (TConjuction xIn) dests, pIn) -> Module (TConjuction xOut) dests
        where
          inputIdx = (inputIdxMaps M.! to) M.! from
          xOut = take inputIdx xIn ++ [pIn] ++ drop (inputIdx + 1) xIn

    -- output signals
    outSignals = case (activeModuleOut, pulse) of
      (Module TBroadcast dests, pIn) -> ([PulseTransmission to dest pIn | dest <- dests])
      (Module (TFlipFlop _) dests, THigh) -> []
      (Module (TFlipFlop x) dests, TLow) -> ([PulseTransmission to dest pOut | dest <- dests])
        where
          pOut = if x then THigh else TLow
      (Module (TConjuction xs) dests, _) -> ([PulseTransmission to dest pOut | dest <- dests])
        where
          pOut = if all (== THigh) xs then TLow else THigh

buttonPulse :: PulseTransmission
buttonPulse = PulseTransmission "button" "broadcaster" TLow

solve1 :: Text -> Int
solve1 ss = cntLow * cntHigh
  where
    (modMapInit, idxMaps) = prepareSystem $ parseInput ss

    (_, cntLow, cntHigh) = iterate pressButton (modMapInit, 0, 0) !! 1000
      where
        pressButton :: (M.Map String Module, Int, Int) -> (M.Map String Module, Int, Int)
        pressButton (modMapIn, cntLow, cntHigh) = (modMapOut, cntLow + cntLowNew, cntHigh + cntHighNew)
          where
            (modMapOut, cntLowNew, cntHighNew) = propagateAndCountSignals modMapIn idxMaps [buttonPulse]

        propagateAndCountSignals :: M.Map String Module -> M.Map String (M.Map String Int) -> [PulseTransmission] -> (M.Map String Module, Int, Int)
        propagateAndCountSignals modMap inputIdxMaps [] = (modMap, 0, 0)
        propagateAndCountSignals modMap inputIdxMaps (x : xs) = (recurMap, newLow + recurCntLow, newHigh + recurCntHigh)
          where
            (newLow, newHigh) = if getPulse x == THigh then (0, 1) else (1, 0)
            (modMapNew, xsNew) = step modMap inputIdxMaps x
            (recurMap, recurCntLow, recurCntHigh) = propagateAndCountSignals modMapNew inputIdxMaps (xs ++ xsNew)

-- Part 2

-- the input is structured so that the 'cs' conjunction module feeds into 'rx', and
-- each flag in 'cs' is periodically activated so that the answer is the least common
-- multiple of the activation periods for each flag.
solve2 :: Text -> Int
solve2 ss = foldl lcm 1 firstActivations
  where
    (modMapInit, idxMaps) = prepareSystem $ parseInput ss

    -- active flags for every cycle
    buttonPressCsIdx = map snd $ iterate pressButton (modMapInit, [])
      where
        pressButton :: (M.Map String Module, [Int]) -> (M.Map String Module, [Int])
        pressButton (modMapIn, _) = propagateAllPulses modMapIn [] [buttonPulse]

        propagateAllPulses :: M.Map String Module -> [Int] -> [PulseTransmission] -> (M.Map String Module, [Int])
        propagateAllPulses modMap csIdx [] = (modMap, csIdx)
        propagateAllPulses modMap csIdx (x : xs) = propagateAllPulses modMapNew csIdxNew (xs ++ xsNew)
          where
            (modMapNew, xsNew) = step modMap idxMaps x
            csIdxNew = nub $ csIdx ++ [i | i <- [0 .. length flags - 1], flags !! i == THigh]
              where
                (Module (TConjuction flags) dests) = modMapNew M.! "cs"

    -- (cycle, cs flags) where ANY index is active
    buttonPressedWithActivations = filter (not . null . snd) $ zip [0 ..] buttonPressCsIdx

    -- first activation cycle for each flag
    firstActivations = map getFirstActive [0 .. length flags - 1]
      where
        Module (TConjuction flags) _ = modMapInit M.! "cs"
        getFirstActive idx = fst $ head $ dropWhile (\(_, x) -> idx `notElem` x) buttonPressedWithActivations
