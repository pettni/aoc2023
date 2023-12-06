{-# LANGUAGE QuasiQuotes #-}

module Day1Spec where

import Data.String.QQ (s)
import Day1 (solve1, solve2)
import Test.Hspec (Spec, it, shouldBe)

example1 =
  [s|
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
|]

example2 =
  [s|
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
|]

spec_day1 :: Spec
spec_day1 = do
  it "part1" $
    solve1 example1 `shouldBe` 142
  it "part2" $
    solve2 example2 `shouldBe` 281
