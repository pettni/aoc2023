{-# LANGUAGE QuasiQuotes #-}

module Day10Spec where

import Data.String.QQ (s)
import Day10 (solve1, solve2)
import Test.Hspec (Spec, it, shouldBe)

example1 =
  [s|
-L|F7
7S-7|
L|7||
-L-J|
L|-JF
|]

example2 =
  [s|
7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ
|]

spec_day10 = do
  it "part1-1" $
    solve1 example1 `shouldBe` 4
  it "part1-2" $
    solve1 example2 `shouldBe` 8
  it "part2" $
    solve2 example1 `shouldBe` 2
