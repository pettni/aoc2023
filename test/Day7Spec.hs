{-# LANGUAGE QuasiQuotes #-}

module Day7Spec where

import Data.String.QQ (s)
import Day7 (solve1, solve2)
import Test.Hspec (Spec, it, shouldBe)

example =
  [s|
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
|]

spec_day7 = do
  it "part1" $
    solve1 example `shouldBe` 6440
  it "part2" $
    solve2 example `shouldBe` 5905
