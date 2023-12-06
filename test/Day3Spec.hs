{-# LANGUAGE QuasiQuotes #-}

module Day3Spec where

import Data.String.QQ
import Day3 (solve1, solve2)
import Test.Hspec (Spec, it, shouldBe)

example =
  [s|
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
|]

spec_day3 :: Spec
spec_day3 = do
  it "part1" $
    solve1 example `shouldBe` 4361
  it "part2" $
    solve2 example `shouldBe` 467835
