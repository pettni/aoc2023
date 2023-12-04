module Day1 (solve1, solve2) where

import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf, tails)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T (lines, unpack)

singleResult :: String -> Int
singleResult s = 10 * fst + snd
  where
    digits = filter isDigit s
    fst = digitToInt . head $ digits
    snd = digitToInt . last $ digits

conv :: String -> Maybe Int
conv [] = Nothing
conv s@(x : _)
  | "one" `isPrefixOf` s = Just 1
  | "two" `isPrefixOf` s = Just 2
  | "three" `isPrefixOf` s = Just 3
  | "four" `isPrefixOf` s = Just 4
  | "five" `isPrefixOf` s = Just 5
  | "six" `isPrefixOf` s = Just 6
  | "seven" `isPrefixOf` s = Just 7
  | "eight" `isPrefixOf` s = Just 8
  | "nine" `isPrefixOf` s = Just 9
  | isDigit x = Just $ digitToInt x
  | otherwise = Nothing

singleResult' :: String -> Int
singleResult' s = 10 * fst + lst
  where
    digits = mapMaybe conv $ tails s
    fst = head digits
    lst = last digits

solve1 :: Text -> Int
solve1 s = sum $ map singleResult input
  where
    input = map T.unpack $ T.lines s

solve2 :: Text -> Int
solve2 s = sum $ map singleResult' input
  where
    input = map T.unpack $ T.lines s
