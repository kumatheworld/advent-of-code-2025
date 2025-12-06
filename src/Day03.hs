module Day03 (solution, part1, part2, tests) where

import AoC.Template (Day (..), readExample, solve)
import qualified Data.Text as T
import Test.Hspec

day :: Day
day = Day 3

maxJoltage' :: [String] -> String -> Int
maxJoltage' acc "" = read $ head acc
maxJoltage' acc (c : s) = maxJoltage' acc' s
  where
    acc' = snd $ foldr step ("", []) acc
    step curr (prev, accTail) = (curr, max curr (prev ++ [c]) : accTail)

maxJoltage :: Int -> String -> Int
maxJoltage n = maxJoltage' (replicate n "")

part :: Int -> T.Text -> Maybe Int
part n = Just . sum . map (maxJoltage n . T.unpack) . T.lines

part1 :: T.Text -> Maybe Int
part1 = part 2

part2 :: T.Text -> Maybe Int
part2 = part 12

solution :: IO ()
solution = solve day part1 part2

-- Tests
tests :: Spec
tests = describe "Day03" $ do
  it "solves part 1 with example 1" $ do
    input <- readExample day 1
    part1 input `shouldBe` Just 357

  it "solves part 2 with example 1" $ do
    input <- readExample day 1
    part2 input `shouldBe` Just 3121910778619
