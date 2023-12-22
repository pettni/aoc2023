{-# LANGUAGE QuasiQuotes #-}

module Day22Spec where

import Data.String.QQ (s)
import Data.Text (Text)
import Day22 (solve1, solve2)
import Test.Hspec (Spec, it, shouldBe)

example =
  [s|
1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9
|]

spec_day20 = do
  it "part1" $
    solve1  example `shouldBe` 5
  it "part1" $
    solve2  example `shouldBe` 7
