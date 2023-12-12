{-# LANGUAGE QuasiQuotes #-}

module Day12Spec where

import Data.String.QQ (s)
import Data.Text (Text)
import Data.Tuple (swap)
import Day12 (solve1, solve2)
import Test.Hspec (Spec, it, shouldBe)


example_row0 =
  [s|
???.### 1,1,3
|]

example_row1 =
  [s|
.??..??...?##. 1,1,3
|]

example_row5 =
  [s|
?###???????? 3,2,1
|]

example =
  [s|
???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
|]

spec_day12 = do
  it "part1-row0" $
    solve1 example_row0 `shouldBe` 1
  it "part1-row1" $
    solve1 example_row1 `shouldBe` 4
  it "part1-row5" $
    solve1 example_row5 `shouldBe` 10
  it "part1" $
    solve1 example `shouldBe` 21
  it "part2" $
    solve2 example `shouldBe` 525152
