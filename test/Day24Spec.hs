{-# LANGUAGE QuasiQuotes #-}

module Day24Spec where

import Data.String.QQ (s)
import Data.Text (Text)
import Day24 (solve1', solve2)
import Test.Hspec (Spec, it, shouldBe)

example =
  [s|
19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3
|]

spec_day24 = do
  it "part1" $
    solve1' ((7, 7), (27, 27)) example `shouldBe` 2
  it "part2" $
    solve2  example `shouldBe` 47
