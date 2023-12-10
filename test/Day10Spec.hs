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

example3 =
  [s|
...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........
|]

example4 =
  [s|
.F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...
|]

example5 =
  [s|
FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L
|]

spec_day10 = do
  it "part1-1" $
    solve1 example1 `shouldBe` 4
  it "part1-2" $
    solve1 example2 `shouldBe` 8
  it "part2-1" $
    solve2 example3 `shouldBe` 4
  it "part2-2" $
    solve2 example4 `shouldBe` 8
  it "part2-3" $
    solve2 example5 `shouldBe` 10
