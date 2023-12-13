{-# LANGUAGE QuasiQuotes #-}

module Day13Spec where

import Data.String.QQ (s)
import Data.Text (Text)
import Data.Tuple (swap)
import Day13 (solve1, solve2)
import Test.Hspec (Spec, it, shouldBe)


example =
  [s|
#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#
|]

spec_day13 = do
  it "part1" $
    solve1 example `shouldBe` 405
  it "part2" $
    solve2 example `shouldBe` 400
