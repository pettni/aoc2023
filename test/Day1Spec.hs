{-# LANGUAGE OverloadedStrings #-}

module Day1Spec (spec) where

import qualified Data.Text as T (Text, unlines)
import Day1 (solve1, solve2)
import Test.Hspec (Spec, it, shouldBe)

example1 :: T.Text
example1 =
  T.unlines
    [ "1abc2",
      "pqr3stu8vwx",
      "a1b2c3d4e5f",
      "treb7uchet"
    ]

example2 :: T.Text
example2 =
  T.unlines
    [ "two1nine",
      "eightwothree",
      "abcone2threexyz",
      "xtwone3four",
      "4nineeightseven2",
      "zoneight234",
      "7pqrstsixteen"
    ]

spec :: Spec
spec = do
  it "part1" $
    solve1 example1 `shouldBe` 142
  it "part2" $
    solve2 example2 `shouldBe` 281
