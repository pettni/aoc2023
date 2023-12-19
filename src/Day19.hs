{-# LANGUAGE TupleSections #-}

module Day19 (solve1, solve2) where

import qualified Data.Map as M
import Data.Text (Text, unpack)
import Data.Tuple (swap)
import Debug.Trace (trace)
import qualified Text.Parsec as Parsec

-- Types

data State = TIn | TReject | TAccept | TWorkflow String deriving (Eq, Show)

data Comp = TLessThan | TGreaterThan deriving (Eq, Show)

data Condition = Condition {target :: Char, comp :: Comp, val :: Int} deriving (Eq, Show)

data Rule = Rule {cond :: Maybe Condition, next :: State} deriving (Eq, Show)

data Workflow = Workflow {name :: String, rules :: [Rule]} deriving (Eq, Show)

data Part = Part {px :: Int, pm :: Int, pa :: Int, ps :: Int} deriving (Eq, Show)

-- Parsing

parseInput :: Text -> ([Workflow], [Part])
parseInput ss = handleErr $ Parsec.parse inputData "Failure" $ unpack ss
  where
    inputData = do
      workflows <- Parsec.sepEndBy1 workflow Parsec.endOfLine
      Parsec.endOfLine
      parts <- Parsec.sepEndBy1 part Parsec.endOfLine
      return (workflows, parts)

    workflow = do
      name <- Parsec.many1 (Parsec.noneOf "{\n")
      rules <- Parsec.between (Parsec.char '{') (Parsec.char '}') (Parsec.sepBy1 rule (Parsec.char ','))
      return (Workflow name rules)

    rule = Parsec.try ruleWithCondition Parsec.<|> ruleWithoutCondition

    ruleWithCondition = do
      condition <- Parsec.many1 (Parsec.noneOf ":,}")
      Parsec.char ':'
      dest <- Parsec.many1 (Parsec.noneOf ",}")
      return (Rule (Just $ parseCondition condition) (parseDest dest))

    ruleWithoutCondition = do
      dest <- Parsec.many1 (Parsec.noneOf ",}")
      return (Rule Nothing (parseDest dest))

    parseDest "R" = TReject
    parseDest "A" = TAccept
    parseDest x = TWorkflow x

    parseCondition :: String -> Condition
    parseCondition x = Condition target op val
      where
        target = head x
        op = parseOp (x !! 1)
        val = read (drop 2 x) :: Int

    parseOp :: Char -> Comp
    parseOp '<' = TLessThan
    parseOp '>' = TGreaterThan

    part = do
      ps <- M.fromList <$> Parsec.between (Parsec.char '{') (Parsec.char '}') (Parsec.sepBy1 partProperty (Parsec.char ','))
      return (Part (ps M.! 'x') (ps M.! 'm') (ps M.! 'a') (ps M.! 's'))

    partProperty = (\x -> (head x, read (drop 2 x) :: Int)) <$> Parsec.many1 (Parsec.noneOf ",}")

    handleErr (Left a) = error (show a)
    handleErr (Right a) = a

-- Part 1

solve1 :: Text -> Int
solve1 ss = sum $ map (\(Part x m a s) -> x + m + a + s) acceptedParts
  where
    (workflows, parts) = parseInput ss
    workflowMap = M.fromList $ (\w@(Workflow name _) -> (name, w)) <$> workflows
    endStates = uncurry (applyWorkflows workflowMap) <$> map (,TIn) parts
    acceptedParts = map snd $ filter ((== TAccept) . fst) (zip endStates parts)

applyRules :: [Rule] -> Part -> State
applyRules (r : rs) part = case r of
  (Rule Nothing next) -> next
  (Rule (Just cond) next) -> (if satisfiesCondition cond part then next else applyRules rs part)
    where
      satisfiesCondition :: Condition -> Part -> Bool
      satisfiesCondition (Condition prop TLessThan val) part = grabProperty prop part < val
      satisfiesCondition (Condition prop TGreaterThan val) part = grabProperty prop part > val

      grabProperty :: Char -> Part -> Int
      grabProperty 'x' = px
      grabProperty 'm' = pm
      grabProperty 'a' = pa
      grabProperty 's' = ps

applyWorkflows :: M.Map String Workflow -> Part -> State -> State
applyWorkflows _ _ TAccept = TAccept
applyWorkflows _ _ TReject = TReject
applyWorkflows workflowMap part TIn = applyWorkflows workflowMap part (TWorkflow "in")
applyWorkflows workflowMap part (TWorkflow workflowName) = applyWorkflows workflowMap part nextState
  where
    nextState = applyRules (rules $ workflowMap M.! workflowName) part

-- Part 2

-- Upper limit not included
data BoxNd = BoxNd {xmin :: [Int], xmax :: [Int]} deriving (Eq, Show)

boxIsect :: BoxNd -> BoxNd -> BoxNd
boxIsect (BoxNd xmin1 xmax1) (BoxNd xmin2 xmax2) = BoxNd (zipWith max xmin1 xmin2) (zipWith min xmax1 xmax2)

boxEmpty :: BoxNd -> Bool
boxEmpty (BoxNd xmin xmax) = or $ zipWith (>=) xmin xmax

boxVolume :: BoxNd -> Int
boxVolume (BoxNd xmin xmax) = foldl (\cum (min, max) -> cum * (max - min)) 1 $ zip xmin xmax

-- (SatisfiedBox, NotSatisfiedBox)
halfPlanes :: Condition -> (BoxNd, BoxNd)
halfPlanes (Condition prop TLessThan val) = (BoxNd [minBound, minBound, minBound, minBound] xlim1, BoxNd xlim2 [maxBound, maxBound, maxBound, maxBound])
  where
    (xlim1, xlim2) = case prop of
      'x' -> ([val, maxBound, maxBound, maxBound], [val, minBound, minBound, minBound])
      'm' -> ([maxBound, val, maxBound, maxBound], [minBound, val, minBound, minBound])
      'a' -> ([maxBound, maxBound, val, maxBound], [minBound, minBound, val, minBound])
      's' -> ([maxBound, maxBound, maxBound, val], [minBound, minBound, minBound, val])
halfPlanes (Condition prop TGreaterThan val) = swap $ halfPlanes (Condition prop TLessThan (val + 1))

solve2 :: Text -> Int
solve2 ss = sum $ boxVolume . fst <$> acceptedFinalSets
  where
    (workflows, parts) = parseInput ss
    workflowMap = M.fromList $ (\w@(Workflow name _) -> (name, w)) <$> workflows
    initSets = [(BoxNd [1, 1, 1, 1] [4001, 4001, 4001, 4001], TIn)]

    finalSets = applyWorkflowsSets workflowMap initSets
    acceptedFinalSets = filter ((== TAccept) . snd) finalSets

applyRulesSets :: [Rule] -> BoxNd -> [(BoxNd, State)]
applyRulesSets (r : rs) box = case r of
  Rule Nothing next -> [(box, next)]
  Rule (Just cond) next -> condResult ++ recurResult
    where
      (boxSat, boxUnsat) = halfPlanes cond
      boxIsectSat = boxIsect box boxSat
      boxIsectUnsat = boxIsect box boxUnsat

      condResult = if boxEmpty boxIsectSat then [] else [(boxIsectSat, next)]
      recurResult = if boxEmpty boxIsectUnsat then [] else applyRulesSets rs boxIsectUnsat

applyWorkflowsSets :: M.Map String Workflow -> [(BoxNd, State)] -> [(BoxNd, State)]
applyWorkflowsSets workflowMap [] = []
applyWorkflowsSets workflowMap ((box, TIn) : xs) = applyWorkflowsSets workflowMap ((box, TWorkflow "in") : xs)
applyWorkflowsSets workflowMap (x@(box, TWorkflow name) : xs) = applyWorkflowsSets workflowMap (xs ++ newStates)
  where
    newStates = applyRulesSets (rules $ workflowMap M.! name) box
applyWorkflowsSets workflowMap (x : xs) = x : applyWorkflowsSets workflowMap xs
