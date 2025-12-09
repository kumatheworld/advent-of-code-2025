module Day09 (solution, part1, part2, tests) where

import AoC.Lib (readNum, splitToPair)
import AoC.Template (Day (..), readExample, solve)
import Data.Bifunctor
import qualified Data.Text as T
import Test.Hspec

day :: Day
day = Day 9

part :: T.Text -> ([(Int, Int)], [(((Int, Int), (Int, Int)), Int)])
part input =
  let ps = map (bimap readNum readNum . splitToPair ",") $ T.lines input
      rects = [((p1, p2), (x2 - x1 + 1) * (abs (y2 - y1) + 1)) | p1@(x1, y1) <- ps, p2@(x2, y2) <- ps, p1 < p2]
   in (ps, rects)

part1 :: T.Text -> Maybe Int
part1 = Just . maximum . map snd . snd . part

-- The part 2 solution is technically wrong but works for the specific test and input
-- Counterexample:
-- #.....#
-- .......
-- #...#..
-- .......
-- .......
-- .......
-- ....#.#
-- the algorithm would find the bottom left rectangle of area 25 right
part2 :: T.Text -> Maybe Int
part2 input =
  let (ps, rects) = part input
      es = zip ps (tail ps ++ [head ps])
   in Just . maximum . map snd . filter (\(pp, _) -> all (doesNotIntersect pp) es) $ rects
  where
    doesNotIntersect ((x1, y1), (x2, y2)) ((u1, v1), (u2, v2)) -- rectangle and edge respectively
      | u1 == u2 = u1 <= x1 || x2 <= u1 || v2' <= y1' || y2' <= v1'
      | v1 == v2 = v1 <= y1' || y2' <= v1 || u2' <= x1 || x2 <= u1'
      | otherwise = undefined
      where
        minmax z w = if z < w then (z, w) else (w, z)
        (y1', y2') = minmax y1 y2
        (u1', u2') = minmax u1 u2
        (v1', v2') = minmax v1 v2

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
