import Test.Hspec

-- Auto-generated imports will be inserted below this marker by the scaffold command
-- AUTOGEN-IMPORTS
import qualified Day06
import qualified Day05
import qualified Day04
import qualified Day03
import qualified Day02
import qualified Day01

main :: IO ()
main = hspec $ do
  describe "Advent of Code Tests" $ do
    -- Auto-generated per-day tests will be inserted below this marker by scaffold
    -- AUTOGEN-TESTS
    Day06.tests
    Day05.tests
    Day04.tests
    Day03.tests
    Day02.tests
    Day01.tests

    it "placeholder test" $ do
      True `shouldBe` True
