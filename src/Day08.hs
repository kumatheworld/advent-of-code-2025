{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Day08 (solution, part1, part2, tests) where

import AoC.Lib (readNum)
import AoC.Template (Day (..), readExample, solve)
import Control.Monad
import Data.Equivalence.Monad
import Data.Graph
import qualified Data.List as L
import qualified Data.Text as T
import Data.Tree
import Test.Hspec

day :: Day
day = Day 8

part :: T.Text -> ([Int], [(Int, Int)])
part input =
  let vs = zip [0 ..] . map (map (readNum @Int) . T.split (== ',')) . T.lines $ input
      xs = map (head . snd) vs
      es = map fst $ L.sortOn snd [((m, n), dist2 p q) | (m, p) <- vs, (n, q) <- vs, m < n]
   in (xs, es)
  where
    dist2 = (sum .) . zipWith (\x y -> (x - y) * (x - y))

part1 :: T.Text -> Maybe Int
part1 input =
  let (xs, es) = part input
      n = length xs
      cs = components . buildG (0, n - 1) . take (if n < 100 then 10 else 1000) $ es
   in Just . product . take 3 . map (length . flatten) . L.sortOn (negate . length) $ cs

-- The credit goes to https://stackoverflow.com/a/4293437
-- I have no idea why it's working
part2 :: T.Text -> Maybe Int
part2 input =
  let (xs, es) = part input
      (i, j) = last $ kruskal es
   in Just $ xs !! i * xs !! j
  where
    run = runEquivM (const ()) (const $ const ())
    kruskal es = run $ filterM go es
      where
        go (u, v) = do
          eq <- equivalent u v
          if eq then return False else equate u v >> return True

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
