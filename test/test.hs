import qualified Day1Spec
import qualified Day2Spec
import qualified Day3Spec
import Test.Hspec (Spec)
import Test.Tasty
import Test.Tasty.Hspec

main = do
  spec1 <- testSpec "day1" Day1Spec.spec
  spec2 <- testSpec "day2" Day2Spec.spec
  spec3 <- testSpec "day3" Day3Spec.spec
  defaultMain (testGroup "tests" [spec1, spec2, spec3])
