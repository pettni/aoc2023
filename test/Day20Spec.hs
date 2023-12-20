{-# LANGUAGE QuasiQuotes #-}

module Day20Spec where

import Data.String.QQ (s)
import Data.Text (Text)
import Day20 (solve1)
import Test.Hspec (Spec, it, shouldBe)

example1 =
  [s|
broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a
|]

example2 =
  [s|
broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output
|]

spec_day20 = do
  it "part1-a" $
    solve1 example1 `shouldBe` 32000000
  it "part1-b" $
    solve1 example2 `shouldBe` 11687500
