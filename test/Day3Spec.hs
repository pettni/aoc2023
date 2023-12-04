{-# LANGUAGE OverloadedStrings #-}

module Day3Spec (spec) where

import qualified Data.Text as T (Text, unlines)
import Day3 (solve1, solve2)
import Test.Hspec (Spec, it, shouldBe)

example :: T.Text
example =
  T.unlines
    [ "467..114..",
      "...*......",
      "..35..633.",
      "......#...",
      "617*......",
      ".....+.58.",
      "..592.....",
      "......755.",
      "...$.*....",
      ".664.598.."
    ]

spec :: Spec
spec = do
  it "part1" $
    solve1 example `shouldBe` 4361
