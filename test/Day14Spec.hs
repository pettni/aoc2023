{-# LANGUAGE QuasiQuotes #-}

module Day14Spec where

import Data.String.QQ (s)
import Data.Text (Text)
import Data.Tuple (swap)
import Day14 (solve1, solve2)
import Test.Hspec (Spec, it, shouldBe)


example =
  [s|
O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....
|]

spec_day14 = do
  it "part1" $
    solve1 example `shouldBe` 136
  it "part2" $
    solve2 example `shouldBe` 64
