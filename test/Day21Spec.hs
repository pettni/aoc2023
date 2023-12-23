{-# LANGUAGE QuasiQuotes #-}

module Day21Spec where

import Data.String.QQ (s)
import Data.Text (Text)
import Day21 (reachableInfinite, solve1')
import Test.Hspec (Spec, it, shouldBe)

example =
  [s|
...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........
|]

spec_day21 = do
  it "part1" $
    solve1' 6 example `shouldBe` 16
  it "part2-reach" $
    reachableInfinite [6, 10, 50, 100] example `shouldBe` [16, 50, 1594, 6536]
