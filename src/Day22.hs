{-# LANGUAGE MultiWayIf #-}

module Day22 (solve1, solve2) where

import Data.List.Extra (nub)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text, unpack)
import qualified Text.Parsec as Parsec

-- Types

data Interval = Interval {getMin :: [Int], getMax :: [Int]} deriving (Eq, Show, Ord)

data IntervalZOrder = TAbove Int | TBelow Int | TOverlap | TDisjoint deriving (Eq, Show)

-- Parsing

parseInput :: Text -> [Interval]
parseInput ss = handleErr $ Parsec.parse inputData "Failure" $ unpack ss
  where
    inputData = Parsec.sepEndBy1 intervals Parsec.endOfLine

    intervals = do
      v1 <- Parsec.sepBy1 number (Parsec.char ',')
      Parsec.char '~'
      v2 <- Parsec.sepBy1 number (Parsec.char ',')
      return (Interval v1 (map (+ 1) v2))

    number = (\x -> read x :: Int) <$> Parsec.many1 Parsec.digit

    handleErr (Left a) = error (show a)
    handleErr (Right a) = a

-- Part 1

zOrder :: Interval -> Interval -> IntervalZOrder
zOrder b1@(Interval min1 max1) b2@(Interval min2 max2) =
  if isEmpty xyIsect
    then TDisjoint
    else
      ( if
          | z1high <= z2low -> TBelow (z2low - z1high)
          | z2high <= z1low -> TAbove (z1low - z2high)
          | otherwise -> TOverlap
      )
  where
    xyIsect@(Interval ismin ismax) = isectIval (Interval (take 2 min1) (take 2 max1)) (Interval (take 2 min2) (take 2 max2))
    xyzIsect = Interval (ismin ++ [minBound]) (ismax ++ [maxBound])
    (Interval [_, _, z1low] [_, _, z1high]) = isectIval b1 xyzIsect
    (Interval [_, _, z2low] [_, _, z2high]) = isectIval b2 xyzIsect

    isectIval :: Interval -> Interval -> Interval
    isectIval (Interval min1 max1) (Interval min2 max2) = Interval (zipWith max min1 min2) (zipWith min max1 max2)

    isEmpty :: Interval -> Bool
    isEmpty (Interval min max) = or $ zipWith (>=) min max

isBelow :: Interval -> Interval -> Bool
isBelow b1 b2 = case zOrder b1 b2 of
  TBelow _ -> True
  _ -> False

dist :: Interval -> Interval -> Int
dist b1 b2 = case zOrder b1 b2 of
  TAbove x -> x
  TBelow x -> x
  TOverlap -> error "Can not compute distance of overlapping intervals"
  TDisjoint -> error "Can not compute distance of disjoint intervals"

solve1 :: Text -> Int
solve1 ss = length fallenBricksMap - length singleSupportBricks
  where
    intervals = parseInput ss
    bricksMap = M.fromList $ zip [0 ..] intervals

    -- let bricks fall to the ground
    fallenBricksMap = letBricksFall bricksMap

    -- find supporting bricks for each brick
    supports =
      filter
        (not . null)
        [ [ otherId
            | (otherId, other) <- M.assocs fallenBricksMap,
              otherId /= brickId,
              zOrder other brick == TBelow 0
          ]
          | (brickId, brick) <- M.assocs fallenBricksMap
        ]

    -- find bricks that single-handedly support some other brick
    singleSupportBricks = nub $ concat $ filter ((== 1) . length) supports

letBricksFall :: M.Map Int Interval -> M.Map Int Interval
letBricksFall bricksMap = fst $ moveDown bricksMap mempty
  where
    belowMaps = M.fromList [(brickId, S.fromList [otherId | (otherId, other) <- M.assocs bricksMap, isBelow other brick]) | (brickId, brick) <- M.assocs bricksMap]

    moveDown :: M.Map Int Interval -> S.Set Int -> (M.Map Int Interval, S.Set Int)
    moveDown bricksMap alreadyPushed
      | S.size alreadyPushed == M.size bricksMap = (bricksMap, alreadyPushed)
      | otherwise = moveDown newBricksMap (S.insert toPushIdx alreadyPushed)
      where
        toPushIdx = head [i | i <- M.keys bricksMap, i `notElem` alreadyPushed, S.isSubsetOf (belowMaps M.! i) alreadyPushed]
        toPush@(Interval [xm, ym, zm] [xp, yp, zp]) = bricksMap M.! toPushIdx

        pushedBrick = Interval [xm, ym, zm - dz] [xp, yp, zp - dz]
          where
            bricksBelow = belowMaps M.! toPushIdx
            dz =
              if S.null bricksBelow
                then zm - 1
                else minimum [dist toPush other | other <- S.toList (S.map (bricksMap M.!) bricksBelow)]

        newBricksMap = M.insert toPushIdx pushedBrick bricksMap

-- Part 2

solve2 :: Text -> Int
solve2 ss = sum $ map (countChainReaction belowSets aboveSets) [0 .. length bricksMap - 1]
  where
    bricksMap = M.fromList $ zip [0 ..] $ parseInput ss :: M.Map Int Interval

    -- let bricks fall to the ground
    fallenBricksMap = letBricksFall bricksMap

    -- for each brick, the bricks that it rests on
    belowSets =
      [ S.fromList
          [otherId | (otherId, other) <- M.assocs fallenBricksMap, zOrder other brick == TBelow 0]
        | (brickId, brick) <- M.assocs fallenBricksMap
      ]

    -- for each brick, the bricks that rest on top of it
    aboveSets =
      [ S.fromList
          [otherId | (otherId, other) <- M.assocs fallenBricksMap, zOrder other brick == TAbove 0]
        | (brickId, brick) <- M.assocs fallenBricksMap
      ]

countChainReaction :: [S.Set Int] -> [S.Set Int] -> Int -> Int
countChainReaction belowSets aboveSets idx = go (S.toList $ aboveSets !! idx) (S.singleton idx)
  where
    go :: [Int] -> S.Set Int -> Int
    go [] _ = 0
    go (x : xs) fallenBricks
      | x `S.member` fallenBricks = go xs fallenBricks -- x already processed
      | null belowX = 1 + go (xs ++ S.toList aboveX) (S.insert x fallenBricks) -- x falls
      | otherwise = go xs fallenBricks -- x still has support
      where
        belowX = S.difference (belowSets !! x) fallenBricks
        aboveX = S.difference (aboveSets !! x) fallenBricks
