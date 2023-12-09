{-# LANGUAGE QuasiQuotes #-}

module Day9Spec where

import Data.String.QQ (s)
import Day9 (solve1, solve2)
import Test.Hspec (Spec, it, shouldBe)

example =
  [s|
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
|]

spec_day8 = do
  it "part1" $
    solve1 example `shouldBe` 114
  it "part2" $
    solve2 example `shouldBe` 2
