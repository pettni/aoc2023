module Day3 (solve1, solve2) where

import Data.Text (Text)
import qualified Data.Text as T (lines, unpack)
import Debug.Trace (trace)

solve1 :: Text -> Int
solve1 ss = solve1' $ map T.unpack $ T.lines ss

solve1' :: [String] -> Int
solve1' ss = length indices
  where
    indices = [(i, j) | i <- [0 .. length ss - 1], j <- [0 .. length (ss !! i) - 1]]

solve2 :: Text -> Int
solve2 ss = 0
