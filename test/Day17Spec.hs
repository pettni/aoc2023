{-# LANGUAGE QuasiQuotes #-}

module Day17Spec where

import Data.String.QQ (s)
import Data.Text (Text)
import Data.Tuple (swap)
import Day17 (solve1, solve2)
import Test.Hspec (Spec, it, shouldBe)


example =
  [s|
2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533
|]

example' =
  [s|
111111111111
999999999991
999999999991
999999999991
999999999991
|]

spec_day17 = do
  it "part1" $
    solve1 example `shouldBe` 102
  it "part2" $
    solve2 example `shouldBe` 94
  it "part2'" $
    solve2 example' `shouldBe` 71
