module Day06 (solution, part1, part2, tests) where

import AoC.Lib (readNum)
import AoC.Template (Day (..), readExample, solve)
import qualified Data.List as L
import Data.List.Split
import qualified Data.Text as T
import Test.Hspec

day :: Day
day = Day 6

-- Homework: unify the two parts

part1 :: T.Text -> Maybe Int
part1 = fmap sum . mapM compute . L.transpose . map T.words . reverse . T.lines
  where
    compute [] = Nothing
    compute (opT : valuesT) =
      case T.unpack opT of
        "+" -> Just . sum $ map readNum valuesT
        "*" -> Just . product $ map readNum valuesT
        _ -> Nothing

part2 :: T.Text -> Maybe Int
part2 = fmap sum . mapM compute . splitOn [T.empty] . map T.strip . T.transpose . T.lines
  where
    compute [] = Nothing
    compute (t : ts) = case T.unsnoc t of
      Just (t1, '+') -> Just . sum $ map readNum (t1 : ts)
      Just (t1, '*') -> Just . product $ map readNum (t1 : ts)
      _ -> Nothing

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
