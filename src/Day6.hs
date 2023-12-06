module Day6 (solve1, solve2) where

import Data.Either (fromRight)
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import qualified Text.Parsec as Parsec

-- Types

data Race = Race {time :: Int, distance :: Int} deriving (Show)

-- Common

parseInput :: Text -> [Race]
parseInput ss = fromRight (error "Failed to parse") $ Parsec.parse dataFile "Failure" $ T.unpack ss
  where
    dataFile = do
      Parsec.string "Time:"
      Parsec.spaces
      times <- intLine
      Parsec.newline
      Parsec.string "Distance:"
      Parsec.spaces
      distances <- intLine
      Parsec.newline
      Parsec.eof
      return $ uncurry Race <$> zip times distances

    intLine = fmap (\x -> read x :: Int) <$> Parsec.sepBy1 (Parsec.many1 Parsec.digit) (Parsec.many1 $ Parsec.char ' ')

-- Part 1

solve1 :: Text -> Int
solve1 ss = product $ numWaysToWin <$> parseInput ss

numWaysToWin :: Race -> Int
numWaysToWin race = length [t | t <- [1 .. rT], t * (rT - t) > distance race]
  where
    rT = time race

-- Part 2

solve2 :: Text -> Int
solve2 ss = numWaysToWin largeRace
  where
    races = parseInput ss
    lT = read (concatMap (show . time) races) :: Int
    lD = read (concatMap (show . distance) races) :: Int
    largeRace = Race lT lD
