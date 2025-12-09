module Day09 (solution, part1, part2, tests) where

import AoC.Lib (readNum, splitToPair)
import AoC.Template (Day (..), readExample, solve)
import Data.Bifunctor
import qualified Data.Text as T
import Test.Hspec

day :: Day
day = Day 9

part1 :: T.Text -> Maybe Int
part1 input =
  let ps = map (bimap readNum readNum . splitToPair ",") $ T.lines input
      areas = [(x2 - x1 + 1) * abs (y2 - y1 + 1) | p1@(x1, y1) <- ps, p2@(x2, y2) <- ps, p1 < p2]
   in Just $ maximum areas

part2 :: T.Text -> Maybe Int
part2 _ = Nothing

solution :: IO ()
solution = solve day part1 part2

-- Tests
tests :: Spec
tests = describe "Day09" $ do
  it "solves part 1 with example 1" $ do
    input <- readExample day 1
    part1 input `shouldBe` Just 50

  it "solves part 2 with example 1" $ do
    input <- readExample day 1
    part2 input `shouldBe` Just 24
