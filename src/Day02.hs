module Day02 (solution, part1, part2, tests) where

import AoC.Lib (readNum)
import AoC.Template (Day (..), readExample, solve)
import Data.List.Split
import qualified Data.Text as T
import Test.Hspec

day :: Day
day = Day 2

allEqual :: (Eq a) => [a] -> Bool
allEqual [] = True
allEqual (x : xs) = all (== x) xs

isInvalid1 :: (Show a) => a -> Bool
isInvalid1 n = let nStr = show n in let (former, latter) = splitAt (length nStr `div` 2) nStr in former == latter

isInvalid2 :: (Show a) => a -> Bool
isInvalid2 n = let nStr = show n in any (allEqual . flip chunksOf nStr) [1 .. length nStr `div` 2]

rangeFromDoubleton :: (Integral a) => [a] -> [a]
rangeFromDoubleton [m, n] = [m .. n]
rangeFromDoubleton _ = undefined

part :: (Int -> Bool) -> T.Text -> Maybe Int
part isInvalid = Just . sum . concatMap (filter isInvalid . rangeFromDoubleton . map readNum . T.split (== '-')) . T.split (== ',')

part1 :: T.Text -> Maybe Int
part1 = part isInvalid1

part2 :: T.Text -> Maybe Int
part2 = part isInvalid2

solution :: IO ()
solution = solve day part1 part2

-- Tests
tests :: Spec
tests = describe "Day02" $ do
  it "solves part 1 with example 1" $ do
    input <- readExample day 1
    part1 input `shouldBe` Just 1227775554

  it "solves part 2 with example 1" $ do
    input <- readExample day 1
    part2 input `shouldBe` Just 4174379265
