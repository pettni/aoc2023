{-# LANGUAGE QuasiQuotes #-}

module Day6Spec where

import Data.String.QQ (s)
import Day6 (solve1, solve2)
import Test.Hspec (Spec, it, shouldBe)

example =
  [s|
Time:      7  15   30
Distance:  9  40  200
|]

spec_day6 = do
  it "part1" $
    solve1 example `shouldBe` 288
  it "part2" $
    solve2 example `shouldBe` 71503
