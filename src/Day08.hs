{-# LANGUAGE TypeApplications #-}

module Day08 (solution, part1, part2, tests) where

import AoC.Lib (readNum)
import AoC.Template (Day (..), readExample, solve)
import Data.Graph
import qualified Data.List as L
import qualified Data.Text as T
import Data.Tree
import Test.Hspec

day :: Day
day = Day 8

part1 :: T.Text -> Maybe Int
part1 input =
  let vs = zip [1 ..] . map (map (readNum @Int) . T.split (== ',')) . T.lines $ input
      ds = [((m, n), dist2 xs ys) | (m, xs) <- vs, (n, ys) <- vs, m < n]
      es = take (if length vs < 100 then 10 else 1000) . map fst . L.sortOn snd $ ds
      cs = L.sortOn (negate . length) . components $ buildG (1, length vs) es
   in Just . product . take 3 . map (length . flatten) $ cs
  where
    dist2 xs ys = foldr (\(x, y) -> ((x - y) * (x - y) +)) 0 $ zip xs ys

part2 :: T.Text -> Maybe Int
part2 _ = Nothing

solution :: IO ()
solution = solve day part1 part2

-- Tests
tests :: Spec
tests = describe "Day08" $ do
  it "solves part 1 with example 1" $ do
    input <- readExample day 1
    part1 input `shouldBe` Just 40

  it "solves part 2 with example 1" $ do
    input <- readExample day 1
    part2 input `shouldBe` Just 25272
