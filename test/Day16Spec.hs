{-# LANGUAGE QuasiQuotes #-}

module Day16Spec where

import Data.String.QQ (s)
import Data.Text (Text)
import Data.Tuple (swap)
import Day16 (solve1, solve2)
import Test.Hspec (Spec, it, shouldBe)


example =
  [s|
.|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....
|]

spec_day16 = do
  it "part1" $
    solve1 example `shouldBe` 46
  it "part2" $
    solve2 example `shouldBe` 51
