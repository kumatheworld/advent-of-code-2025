module Day04 (solution, part1, part2, tests) where

import AoC.Lib (readMatrix)
import AoC.Template (Day (..), readExample, solve)
import Data.Matrix
import qualified Data.Text as T
import Test.Hspec

day :: Day
day = Day 4

neighbors :: (Int, Int) -> Matrix a -> [Maybe a]
neighbors ij mat =
  [ safeGet i' j' mat
    | di <- [-1 .. 1],
      dj <- [-1 .. 1],
      (di, dj) /= (0, 0),
      let i' = fst ij + di,
      let j' = snd ij + dj
  ]

part1 :: T.Text -> Maybe Int
part1 input = Just . length $ filter accessible ijs
  where
    accessible ij = mat ! ij == '@' && length (filter (== Just '@') (neighbors ij mat)) < 4
    ijs = [(i, j) | i <- [1 .. nrows mat], j <- [1 .. ncols mat]]
    mat = readMatrix input

part2 :: T.Text -> Maybe Int
part2 _ = Nothing

solution :: IO ()
solution = solve day part1 part2

-- Tests
tests :: Spec
tests = describe "Day04" $ do
  it "solves part 1 with example 1" $ do
    input <- readExample day 1
    part1 input `shouldBe` Just 13

  it "solves part 2 with example 1" $ do
    input <- readExample day 1
    part2 input `shouldBe` Just 43
