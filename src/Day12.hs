module Day12 (solution, part1, part2, tests) where

import AoC.Lib (readNum, splitToPair)
import AoC.Template (Day (..), readExample, solve)
import Data.Bifunctor
import qualified Data.List as L
import qualified Data.Text as T
import Test.Hspec

day :: Day
day = Day 12

-- This just outputs an upper bound
-- A region cannot fit all of the presents if the total present size exceeds the region size, but the converse isn't always true
-- The third region of the example is a counterexample
-- But it turns out that my real input didn't have any counterexample
part1 :: T.Text -> Maybe Int
part1 input =
  let chunks = T.splitOn (T.pack "\n\n") $ T.strip input
      (shapesT, regionsT) = L.splitAt (length chunks - 1) chunks
      shapes = do
        block <- map T.unpack shapesT
        let llb = map (map (== '#')) . tail $ lines block
        -- So actually the process of computing rotates and flips are unnecessary
        -- But I leave this here to prove a bit of my effort...
        return . L.nub $ [id, reverse . L.transpose, reverse . map reverse, L.transpose . reverse] <*> [reverse llb, llb]
      regions = do
        line <- T.lines $ head regionsT
        let (whS, qsS) = splitToPair ": " line
            wh = bimap readNum readNum $ splitToPair "x" whS
            qs = map readNum $ T.words qsS
        return (wh, qs)
      sizes = map (length . concatMap (filter id) . head) shapes
      uBound = length . filter id $ map (\((w, h), qs) -> sum (zipWith (*) qs sizes) <= w * h) regions
   in Just uBound

-- I have an idea about computing a lower bound, which basically would go like the following:
-- Basically you try to fill a shape in a region from the top left to the bottom right in a greedy way
-- You would choose a biggest one that still fits, and you might end up choosing the wrong one
-- The order of traversal also matters; if you go row by row, you might miss the example 1 situation
-- Perhaps going diagonally from the top right to the bottom left is a good idea?

part2 :: T.Text -> Maybe Int
part2 _ = Nothing

solution :: IO ()
solution = solve day part1 part2

-- Tests
tests :: Spec
tests = describe "Day12" $ do
  it "solves part 1 with example 1" $ do
    input <- readExample day 1
    part1 input `shouldBe` Just 2
