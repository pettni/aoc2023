{-# LANGUAGE QuasiQuotes #-}

module Day8Spec where

import Data.String.QQ (s)
import Day8 (solve1, solve2)
import Test.Hspec (Spec, it, shouldBe)

example1 =
  [s|
RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
|]

example2 =
  [s|
LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
|]

example3 =
  [s|
LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
|]

spec_day8 = do
  it "part1-1" $
    solve1 example1 `shouldBe` 2
  it "part1-2" $
    solve1 example2 `shouldBe` 6
  it "part2" $
    solve2 example3 `shouldBe` 6
