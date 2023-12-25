{-# LANGUAGE QuasiQuotes #-}

module Day25Spec where

import Data.String.QQ (s)
import Data.Text (Text)
import Day25 (solve1)
import Test.Hspec (Spec, it, shouldBe)

example =
  [s|
jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr
|]

spec_day25 = do
  it "part1" $
    solve1 example `shouldBe` 54
