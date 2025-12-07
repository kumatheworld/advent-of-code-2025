module Day06 (solution, part1, part2, tests) where

import AoC.Lib (readNum)
import AoC.Template (Day (..), readExample, solve)
import qualified Data.List as L
import qualified Data.Text as T
import Test.Hspec

day :: Day
day = Day 6

reduce :: [T.Text] -> Maybe Int
reduce [] = Nothing
reduce (opT : valuesT) =
  let op = case T.unpack opT of
        "+" -> (+)
        "*" -> (*)
        _ -> undefined
   in Just $ foldl1 op (map readNum valuesT)

part1 :: T.Text -> Maybe Int
part1 = fmap sum . mapM (reduce . reverse) . L.transpose . map T.words . T.lines

part2 :: T.Text -> Maybe Int
part2 _ = Nothing

solution :: IO ()
solution = solve day part1 part2

-- Tests
tests :: Spec
tests = describe "Day06" $ do
  it "solves part 1 with example 1" $ do
    input <- readExample day 1
    part1 input `shouldBe` Just 4277556

  it "solves part 2 with example 1" $ do
    input <- readExample day 1
    part2 input `shouldBe` Just 3263827
