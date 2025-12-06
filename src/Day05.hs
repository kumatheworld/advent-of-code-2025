module Day05 (solution, part1, part2, tests) where

import AoC.Template (Day (..), readExample, solve)
import qualified Data.Text as T
import Test.Hspec

day :: Day
day = Day 5

part1 :: T.Text -> Maybe Int
part1 _ = Nothing

part2 :: T.Text -> Maybe Int
part2 _ = Nothing

solution :: IO ()
solution = solve day part1 part2

-- Tests
tests :: Spec
tests = describe "Day05" $ do
  it "solves part 1 with example 1" $ do
    input <- readExample day 1
    part1 input `shouldBe` Just 3

  -- Uncomment to test with additional examples (create files like 05-2.txt, 05-3.txt, etc.)
  -- it "solves part 1 with example 2" $ do
  --   input <- readExample day 2
  --   part1 input `shouldBe` Just 0

  it "solves part 2 with example 1" $ do
    input <- readExample day 1
    part2 input `shouldBe` Just 0 -- TODO: Replace with expected value
