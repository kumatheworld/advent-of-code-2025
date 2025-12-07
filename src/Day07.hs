module Day07 (solution, part1, part2, tests) where

import AoC.Template (Day (..), readExample, solve)
import qualified Data.List as L
import qualified Data.Text as T
import Test.Hspec

day :: Day
day = Day 7

part1 :: T.Text -> Maybe Int
part1 input = do
  let ls = T.lines input
  start <- T.findIndex (== 'S') $ head ls
  return $ numSplits [start] (tail . tail $ ls)
  where
    numSplits _ [] = 0
    numSplits beams (splittersT : rest) = n + numSplits beams' (tail rest)
      where
        n = length . filter ((== 2) . length) $ splitBeams
        beams' = L.nub $ concat splitBeams
        splitBeams = forward (L.elemIndices '^' $ T.unpack splittersT) beams
        forward [] bs = map (: []) bs
        forward _ [] = []
        forward ss@(s : ss') bs@(b : bs') = case compare s b of
          LT -> forward ss' bs
          EQ -> [b - 1, b + 1] : forward ss' bs'
          GT -> [b] : forward ss bs'

part2 :: T.Text -> Maybe Int
part2 _ = Nothing

solution :: IO ()
solution = solve day part1 part2

-- Tests
tests :: Spec
tests = describe "Day07" $ do
  it "solves part 1 with example 1" $ do
    input <- readExample day 1
    part1 input `shouldBe` Just 21

  it "solves part 2 with example 1" $ do
    input <- readExample day 1
    part2 input `shouldBe` Just 40
