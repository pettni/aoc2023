{-# LANGUAGE QuasiQuotes #-}

module Day11Spec where

import Data.String.QQ (s)
import Data.Text (Text)
import Data.Tuple (swap)
import Day11 (solve1, solve2')
import Test.Hspec (Spec, it, shouldBe)

example =
  [s|
...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
|]

spec_day11 = do
  it "part1" $
    solve1 example `shouldBe` 374
  it "part2-a" $
    solve2' 2 example `shouldBe` 374
  it "part2-b" $
    solve2' 10 example `shouldBe` 1030
  it "part2-c" $
    solve2' 100 example `shouldBe` 8410
