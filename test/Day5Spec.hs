{-# LANGUAGE QuasiQuotes #-}

module Day5Spec where

import Data.String.QQ (s)
import Day5 (Interval (..), SingleRangeMap (..), mapIvals, solve1, solve2)
import Test.Hspec (Spec, it, shouldBe)

example =
  [s|
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
|]

spec_day5 = do
  it "part1" $
    solve1 example `shouldBe` 35
  it "part2" $
    solve2 example `shouldBe` 46

spec_ival = do
  it "map1" $
    mapIvals rangeMap1 ivals_in1 `shouldBe` ivals_out1
  it "map2" $
    mapIvals rangeMap2 ivals_in2 `shouldBe` ivals_out2
  it "map2" $
    mapIvals rangeMap3 ivals_in3 `shouldBe` ivals_out3
  where
    rangeMap1 = [SingleRangeMap 10 5 3]
    ivals_in1 = [Interval 3 10]
    ivals_out1 = [Interval 3 4, Interval 8 12]

    rangeMap2 = [SingleRangeMap 10 5 3, SingleRangeMap 26 10 3]
    ivals_in2 = [Interval 3 10, Interval 12 20]
    ivals_out2 = [Interval 3 4, Interval 8 20, Interval 26 26, Interval 28 28]

    rangeMap3 = [SingleRangeMap 50 98 2, SingleRangeMap 52 50 48]
    ivals_in3 = [Interval 55 67, Interval 79 92]
    ivals_out3 = [Interval 57 69, Interval 81 94]
