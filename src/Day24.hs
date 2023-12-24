{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NumericUnderscores #-}

module Day24 (solve1, solve1', solve2) where

import Control.Applicative (liftA2)
import qualified Data.Array.Unboxed as A
import Data.List (intercalate)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text, unpack)
import qualified Text.Parsec as Parsec

-- Types

newtype Vec a = Vec {getArr :: A.Array Int a} deriving (Eq, Ord, Foldable, Functor)

createVec :: [a] -> Vec a
createVec xs = Vec $ A.listArray (0, length xs - 1) xs

vecToList :: Vec a -> [a]
vecToList (Vec xs) = [xs A.! i | i <- A.range (A.bounds xs)]

instance (Show a) => Show (Vec a) where
  show (Vec xs) = "[" ++ inner ++ "]"
    where
      inner = intercalate ", " . map show . A.elems $ xs

zipWithArr :: (A.Ix i) => (a -> b -> c) -> A.Array i a -> A.Array i b -> A.Array i c
zipWithArr f xs ys = A.listArray (A.bounds xs) $ fmap (liftA2 f (xs A.!) (ys A.!)) (A.range (A.bounds xs))

instance (Num a) => Num (Vec a) where
  Vec xs + Vec xs' = Vec $ zipWithArr (+) xs xs'
  Vec xs - Vec xs' = Vec $ zipWithArr (-) xs xs'
  Vec xs * Vec xs' = Vec $ zipWithArr (*) xs xs'
  abs (Vec xs) = Vec $ fmap abs xs
  signum (Vec xs) = Vec $ fmap signum xs
  fromInteger i = Vec $ A.listArray (0, 1) [fromInteger i]

instance (Fractional a) => Fractional (Vec a) where
  Vec xs / Vec xs' = Vec $ zipWithArr (/) xs xs'
  fromRational f = Vec $ A.listArray (0, 1) [fromRational f]

class VectorNum v where
  mul :: (Num a) => a -> v a -> v a
  dot :: (Num a) => v a -> v a -> a
  cross2 :: (Num a) => v a -> v a -> a
  cross3 :: (Num a) => v a -> v a -> v a
  norm :: (Floating a) => v a -> a

instance VectorNum Vec where
  t `mul` Vec xs = Vec $ (* t) <$> xs
  v `dot` v' = sum $ v * v'
  Vec xs `cross2` Vec xs' = x1 * y2 - y1 * x2
    where
      [x1, y1] = (xs A.!) <$> [0, 1]
      [x2, y2] = (xs' A.!) <$> [0, 1]
  Vec xs `cross3` Vec xs' = Vec $ A.listArray (0, 2) [y1 * z2 - z1 * y2, z1 * x2 - x1 * z2, x1 * y2 - y1 * x2]
    where
      [x1, y1, z1] = (xs A.!) <$> [0, 1, 2]
      [x2, y2, z2] = (xs' A.!) <$> [0, 1, 2]

  norm v = sqrt $ v `dot` v

data Particle a = Particle {getPos :: Vec a, getVel :: Vec a} deriving (Eq, Show, Ord)

-- Parsing

parseInput :: Text -> [Particle Int]
parseInput ss = handleErr $ Parsec.parse inputData "Failure" $ unpack ss
  where
    inputData = Parsec.sepEndBy1 particle Parsec.endOfLine

    particle = do
      p <- vec3
      do
        Parsec.spaces
        Parsec.char '@'
        Parsec.spaces
      Particle p <$> vec3

    vec3 = createVec <$> Parsec.sepBy1 number commaSpace

    commaSpace = do
      Parsec.char ','
      Parsec.spaces

    number = (\x -> read x :: Int) <$> Parsec.many1 (Parsec.noneOf ", \n")

    handleErr (Left a) = error (show a)
    handleErr (Right a) = a

-- Part 1

project2d :: Particle a -> Particle a
project2d (Particle (Vec p) (Vec v)) =
  Particle
    (Vec (A.listArray (0, 1) [p A.! 0, p A.! 1]))
    (Vec (A.listArray (0, 1) [v A.! 0, v A.! 1]))

isect2d :: Particle Float -> Particle Float -> Maybe (Vec Float)
isect2d (Particle p v) (Particle p' v')
  | abs (cross2 v v') < 1e-8 = Nothing
  | t >= 0 && t' >= 0 = Just $ p + t `mul` v
  | otherwise = Nothing
  where
    t = ((v' `cross2` p') - (v' `cross2` p)) / (v' `cross2` v)
    t' = ((v `cross2` p) - (v `cross2` p')) / (v `cross2` v')

solve1' :: ((Float, Float), (Float, Float)) -> Text -> Int
solve1' ((xm, ym), (xp, yp)) ss = length validIntersections
  where
    particles2d = map ((\(Particle p v) -> Particle (fmap fromIntegral p) (fmap fromIntegral v)) . project2d) (parseInput ss)
    validIntersections = filter isInside $ catMaybes [isect2d p1 p2 | p1 <- particles2d, p2 <- particles2d, p1 < p2]
      where
        isInside :: Vec Float -> Bool
        isInside (Vec v) = xm <= x && x <= xp && ym <= y && y <= yp
          where
            (x, y) = (v A.! 0, v A.! 1)

solve1 :: Text -> Int
solve1 = solve1' ((200_000_000_000_000, 200_000_000_000_000), (400_000_000_000_000, 400_000_000_000_000))

-- Part 2

-- Check if there is a solution for a fixed (vx, vy) using two rays
findCandidate :: Int -> Int -> Particle Int -> Particle Int -> Maybe (Particle Int)
findCandidate vxi vyi par0i par1i = if isConsistent then Just (Particle (fmap fromInteger p) (fmap fromInteger v)) else Nothing
  where
    nonZero x = if x == 0 then 1 else x

    -- Use unbounded Integer type because these numbers get large..
    (vx, vy) = (toInteger vxi, toInteger vyi)
    p0@[x0, y0, z0] = map toInteger $ vecToList $ getPos par0i
    p1@[x1, y1, z1] = map toInteger $ vecToList $ getPos par1i
    v0@[vx0, vy0, vz0] = map toInteger $ vecToList $ getVel par0i
    v1@[vx1, vy1, vz1] = map toInteger $ vecToList $ getVel par1i

    -- Four equations, four unknowns: x, y, t0, t1
    --  x + t0 (vx - vx0) = x0      [1 0 a 0] [x]    [x0]
    --  y + t0 (vy - vy0) = y0  ==> [0 1 b 0] [y]  = [y0]
    --  x + t1 (vx - vx1) = x1      [1 0 0 c] [t0]   [x1]
    --  y + t1 (vy - vy1) = y1      [0 1 0 d] [t1]   [y1]
    (a, b, c, d) = (vx - vx0, vy - vy0, vx - vx1, vy - vy1)
    det = a * d - b * c
    x = (-b * c * x0 + a * c * y0 + a * d * x1 - a * c * y1) `div` nonZero det
    y = (-b * d * x0 + a * d * y0 + b * d * x1 - b * c * y1) `div` nonZero det
    t0 = (d * x0 - c * y0 - d * x1 + c * y1) `div` nonZero det
    t1 = (b * x0 - a * y0 - b * x1 + a * y1) `div` nonZero det

    -- Solve for (z, vz) using
    --     z + t0 vz = z0 + t0 vz0
    --     z + t1 vz = z1 + t1 vz1
    vz = ((z1 - z0) + t1 * vz1 - t0 * vz0) `div` nonZero (t1 - t0)
    z = (t1 * (z0 + t0 * vz0) - t0 * (z1 + t1 * vz1)) `div` nonZero (t1 - t0)

    p = createVec [x, y, z]
    v = createVec [vx, vy, vz]

    isConsistent =
      p + t0 `mul` v == createVec p0 + t0 `mul` createVec v0
        && p + t1 `mul` v == createVec p1 + t1 `mul` createVec v1

hasIntersection :: Particle Int -> Particle Int -> Bool
hasIntersection (Particle p v) (Particle p' v') = not (null tCands) && isGood (head tCands)
  where
    -- find candidate times from p + t v = p' + t v'
    tCands = map (uncurry div) . filter ((/= 0) . snd) $ zip (vecToList $ p - p') (vecToList $ v' - v)
    -- need p + t v = p' + t v'  ==> p-p' = t (v' - v)
    isGood :: Int -> Bool
    isGood tCand = tCand >= 0 && p + tCand `mul` v == p' + tCand `mul` v'

isSolution :: [Particle Int] -> Particle Int -> Bool
isSolution particles candidate = all (hasIntersection candidate) particles

solve2 :: Text -> Int
solve2 ss = (sum . getPos) $ head sols
  where
    particles = parseInput ss
    tryCombinations = [(vx, vy, par1, par2) | par1 <- particles, par2 <- particles, par1 < par2, vx <- [-250 .. 250], vy <- [-250 .. 250]]
    sols =
      filter (isSolution particles)
        . mapMaybe (\(vx, vy, par1, par2) -> findCandidate vx vy par1 par2)
        $ tryCombinations
