module Day05 (solution, part1, part2, tests) where

import AoC.Lib (readNum, splitToPair)
import AoC.Template (Day (..), readExample, solve)
import Data.Bifunctor
import qualified Data.List as L
import qualified Data.Text as T
import Test.Hspec

day :: Day
day = Day 5

part :: T.Text -> ([(Int, Int)], T.Text)
part input =
  let (rangesT, idsT) = splitToPair "\n\n" input
      rangesMessed = map (bimap readNum readNum . splitToPair "-") $ T.lines rangesT
      ranges = foldr step [] $ L.sortOn (\(x, y) -> (y, x)) rangesMessed
   in (ranges, idsT)
  where
    step xy [] = [xy]
    step xy@(x, y) xys@((x', y') : xys')
      | x >= x' = xys
      | y < x' - 1 = xy : xys
      | otherwise = (x, y') : xys'

part1 :: T.Text -> Maybe Int
part1 input =
  let (ranges, idsT) = part input
      ids = map readNum . T.lines $ idsT
      isFresh n = or $ [l <= n && n <= r | (l, r) <- ranges] -- binary search would be faster but who cares
   in Just . length . filter isFresh $ ids

part2 :: T.Text -> Maybe Int
part2 = Just . sum . map (\(l, r) -> r - l + 1) . fst . part

solution :: IO ()
solution = solve day part1 part2

-- Tests
tests :: Spec
tests = describe "Day05" $ do
  it "solves part 1 with example 1" $ do
    input <- readExample day 1
    part1 input `shouldBe` Just 3

  it "solves part 2 with example 1" $ do
    input <- readExample day 1
    part2 input `shouldBe` Just 14
