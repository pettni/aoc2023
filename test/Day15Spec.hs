{-# LANGUAGE QuasiQuotes #-}

module Day15Spec where

import Data.String.QQ (s)
import Data.Text (Text)
import Data.Tuple (swap)
import Day15 (solve1, solve2)
import Test.Hspec (Spec, it, shouldBe)


example =
  [s|
rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7
|]

spec_day15 = do
  it "part1" $
    solve1 example `shouldBe` 1320
  it "part2" $
    solve2 example `shouldBe` 145
