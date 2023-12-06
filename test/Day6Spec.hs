{-# LANGUAGE OverloadedStrings #-}

module Day6Spec where

import qualified Data.Text as T (Text, unlines)
import Day6 (solve1, solve2)
import Test.Hspec (Spec, it, shouldBe)

example :: T.Text
example =
  T.unlines
    [ "Time:      7  15   30",
      "Distance:  9  40  200"
    ]

spec_day6 = do
  it "part1" $
    solve1 example `shouldBe` 288
  it "part2" $
    solve2 example `shouldBe` 71503
