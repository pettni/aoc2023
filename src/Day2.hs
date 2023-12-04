module Day2 (solve1, solve2) where

import Data.Char (isDigit)
import Data.List (isSuffixOf)
import Data.List.Split (splitOn)
import Data.Text (Text)
import qualified Data.Text as T (lines, unpack)

data Round = Round {red :: Int, green :: Int, blue :: Int} deriving (Eq, Show)

parseGame :: String -> (Int, [Round])
parseGame s = (game, rounds)
  where
    [game_head, game_tail] = splitOn ": " s
    game = read (reverse $ takeWhile isDigit $ reverse game_head) :: Int
    rounds = map parseRound $ splitOn "; " game_tail

parseRound :: String -> Round
parseRound s = roundMax parts
  where
    parts = map parsePartialRound $ splitOn ", " s

parsePartialRound :: String -> Round
parsePartialRound s
  | "red" `isSuffixOf` s = Round n 0 0
  | "green" `isSuffixOf` s = Round 0 n 0
  | "blue" `isSuffixOf` s = Round 0 0 n
  | otherwise = Round 0 0 0
  where
    n = read $ takeWhile isDigit s

roundMax :: [Round] -> Round
roundMax rs = Round r g b
  where
    r = foldl max 0 $ map red rs
    g = foldl max 0 $ map green rs
    b = foldl max 0 $ map blue rs

isValidRound :: Round -> Bool
isValidRound (Round r g b) = r <= 12 && g <= 13 && b <= 14

isValidGame :: [Round] -> Bool
isValidGame = all isValidRound

roundPower :: Round -> Int
roundPower (Round r g b) = r * g * b

gamePower :: [Round] -> Int
gamePower rs = roundPower $ roundMax rs

solve1 :: Text -> Int
solve1 s = sum (map fst valid_games)
  where
    input = map T.unpack $ T.lines s
    games = map parseGame input
    valid_games = filter (isValidGame . snd) games

solve2 :: Text -> Int
solve2 s = sum (map (gamePower . snd) games)
  where
    input = map T.unpack $ T.lines s
    games = map parseGame input
