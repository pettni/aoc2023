module Day9 (solve1, solve2) where

import Control.Exception (throw)
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import qualified Text.Parsec as Parsec

-- Parsing

parseInput :: Text -> [[Int]]
parseInput ss = handleErr $ Parsec.parse inputData "Failure" $ T.unpack ss
  where
    inputData = Parsec.sepEndBy1 intLine Parsec.endOfLine
    intLine = fmap (\x -> read x :: Int) <$> Parsec.sepBy1 (Parsec.many1 (Parsec.oneOf "1234567890-+")) (Parsec.char ' ')

    handleErr (Left a) = error (show a)
    handleErr (Right a) = a

-- Part 1

solve1 :: Text -> Int
solve1 ss = sum $ extrapolateHistory <$> parseInput ss

extrapolateHistory :: [Int] -> Int
extrapolateHistory xs
  | all (== 0) xs = 0
  | otherwise = last xs + extrapolateHistory (zipWith (-) (drop 1 xs) xs)

-- Part 2

solve2 :: Text -> Int
solve2 ss = sum $ extrapolateHistory . reverse <$> parseInput ss
