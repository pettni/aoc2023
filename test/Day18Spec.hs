{-# LANGUAGE QuasiQuotes #-}

module Day18Spec where

import Data.String.QQ (s)
import Data.Text (Text)
import Data.Tuple (swap)
import Day18 (solve1, solve2)
import Test.Hspec (Spec, it, shouldBe)

example =
  [s|
R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)
|]

spec_day18 = do
  it "part1" $
    solve1 example `shouldBe` 62
  it "part2" $
    solve2 example `shouldBe` 952408144115
