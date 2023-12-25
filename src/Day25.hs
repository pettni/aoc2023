module Day25 (solve1, solve2) where

import qualified Data.Array.Unboxed as A
import qualified Data.HashPSQ as PS
import Data.List (nub)
import Data.List.Extra (minimumOn, sort)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Data.Text (Text, unpack)
import qualified Text.Parsec as Parsec

-- Types

type NodeId = String

data Node = Node {getId :: NodeId, getNodeWeight :: Int, getOrigNodes :: [NodeId]} deriving (Eq, Ord)

instance Show Node where
  show (Node n w o) = n ++ "(" ++ show w ++ ")" ++ "[" ++ show o ++ "]"

data Edge = Edge {getFst :: NodeId, getSnd :: NodeId, getEdgeWeight :: Int} deriving (Eq, Ord)

instance Show Edge where
  show (Edge n1 n2 w) = n1 ++ "--" ++ n2 ++ "(" ++ show w ++ ")"

data Graph = Graph {getV :: M.Map NodeId Node, getE :: [Edge]} deriving (Show)

-- nodeId, priority, connectivity
type NodeQueue = PS.HashPSQ NodeId Int Int

-- Parsing

parseInput :: Text -> M.Map String [String]
parseInput ss = handleErr $ Parsec.parse inputData "Failure" $ unpack ss
  where
    inputData = M.fromList <$> Parsec.sepEndBy1 entryLine Parsec.endOfLine

    entryLine = do
      key <- Parsec.many1 (Parsec.noneOf ":\n")
      Parsec.string ": "
      vals <- Parsec.sepBy1 (Parsec.many1 (Parsec.noneOf " \n")) (Parsec.char ' ')
      return (key, vals)

    handleErr (Left a) = error (show a)
    handleErr (Right a) = a

parseGraph :: M.Map String [String] -> Graph
parseGraph inputData = Graph nodes edges
  where
    edges = [Edge n1 n2 1 | (n1, n2s) <- M.assocs inputData, n2 <- n2s]
    nodes = M.fromList $ map (\id -> (id, Node id 1 [id])) $ nub $ map getFst edges ++ map getSnd edges

-- Part 1

-- List of edges for each vertex
createEdgeMap :: Graph -> M.Map NodeId [Edge]
createEdgeMap (Graph nodes edges) = foldl fnFold (M.fromList [(id, []) | id <- M.keys nodes]) edges
  where
    fnFold :: M.Map NodeId [Edge] -> Edge -> M.Map NodeId [Edge]
    fnFold m e@(Edge id0 id1 _) = M.adjust (e :) id0 . M.adjust (e :) id1 $ m

mergeVertices :: Graph -> NodeId -> NodeId -> Graph
mergeVertices (Graph nodes edges) id1 id2 = Graph nodesNew edgesNew
  where
    id12 = id1 ++ "-" ++ id2
    nodesNew = M.delete id1 . M.delete id2 . M.insert id12 (Node id12 w12 o12) $ nodes
      where
        n1 = nodes M.! id1
        n2 = nodes M.! id2
        w12 = getNodeWeight n1 + getNodeWeight n2
        o12 = getOrigNodes n1 ++ getOrigNodes n2
    edgesNew =
      mergeDuplicates -- merge adjacent duplicates
        . sort -- sort so that duplicate edges become adjacent
        . map (\(Edge id1' id2' w) -> Edge (min id1' id2') (max id1' id2') w) -- smallest edge first (for sorting + merging)
        . filter (\(Edge id1' id2' w) -> id1' /= id12 || id2' /= id12) --  remove self edges
        . map (\(Edge id1' id2' w) -> Edge (sub id1') (sub id2') w) -- update node names
        $ edges
      where
        sub x = if x == id1 || x == id2 then id12 else x

        mergeDuplicates :: [Edge] -> [Edge]
        mergeDuplicates [] = []
        mergeDuplicates [x] = [x]
        mergeDuplicates (x@(Edge n11 n12 w1) : y@(Edge n21 n22 w2) : xs) =
          if n11 == n21 && n12 == n22
            then mergeDuplicates $ Edge n11 n12 (w1 + w2) : xs
            else x : mergeDuplicates (y : xs)

getConnectivity :: Graph -> S.Set NodeId -> NodeId -> Int
getConnectivity (Graph _ edges) inNodes x = sum $ [w | (Edge n1 n2 w) <- edges, (n1 `S.member` inNodes && n2 == x) || (n1 == x && n2 `S.member` inNodes)]

-- O(|E|)
getConnectivities :: Graph -> S.Set NodeId -> M.Map NodeId Int
getConnectivities (Graph nodes edges) inNodes = foldl fnFold (M.fromList [(id, 0) | id <- M.keys nodes]) edges
  where
    fnFold :: M.Map NodeId Int -> Edge -> M.Map NodeId Int
    fnFold it (Edge n1 n2 w)
      | n1 `S.member` inNodes && n2 `S.notMember` inNodes = M.adjust (+ w) n2 it
      | n2 `S.member` inNodes && n1 `S.notMember` inNodes = M.adjust (+ w) n1 it
      | otherwise = it

-- Stoer–Wagner min cut algorithm
minCut :: Graph -> ([NodeId], Int)
minCut g = snd . head . dropWhile ((> 1) . length . getV . fst) $ iterate fnIter (g, ([], maxBound))
  where
    fnIter :: (Graph, ([NodeId], Int)) -> (Graph, ([NodeId], Int))
    fnIter (g'@(Graph nodes _), oldCut) = (uncurry (mergeVertices g') toMerge, minimumOn snd [oldCut, (cutNodes, cutSize)])
      where
        (toMerge, (lastNode, cutSize)) = runPhase (createEdgeMap g') (PS.fromList [(k, 0, 0) | k <- M.keys nodes])
        cutNodes = getOrigNodes $ nodes M.! lastNode

    runPhase :: M.Map NodeId [Edge] -> NodeQueue -> ((NodeId, NodeId), (NodeId, Int))
    runPhase edgeMap pq
      | PS.size pq == 2 = ((n', lastNode), (lastNode, lastConnectivity))
      | otherwise = runPhase edgeMap newPq
      where
        (n', _, _) = fromJust $ PS.findMin pq
        newPq = (\q -> foldl fnFold q (edgeMap M.! n')) . PS.delete n' $ pq
          where
            fnFold :: NodeQueue -> Edge -> NodeQueue
            fnFold q (Edge n0 n1 w)
              | n1 `PS.member` q = snd $ PS.alter (\(Just (p, v)) -> (0, Just (p - w, v + w))) n1 q
              | n0 `PS.member` q = snd $ PS.alter (\(Just (p, v)) -> (0, Just (p - w, v + w))) n0 q
              | otherwise = q

        (lastNode, _, lastConnectivity) = fromJust $ PS.findMin newPq -- only used at exit

solve1 :: Text -> Int
solve1 ss = (\x -> x * (length (getV graph) - x)) $ length cut
  where
    graph = parseGraph $ parseInput ss
    (cut, cutWeight) = minCut graph

-- Part 2

solve2 :: Text -> Int
solve2 ss = 0
