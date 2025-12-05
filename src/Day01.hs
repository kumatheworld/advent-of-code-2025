module Day01 (solution, part1, part2, tests) where

import AoC.Lib (readInt)
import AoC.Template (Day (..), readExample, solve)
import Control.Monad
import qualified Data.Text as T
import Test.Hspec

day :: Day
day = Day 1

type Degree = Int

type Password = Int

type State = (Degree, Password)

dLimit :: Degree
dLimit = 100

start :: State
start = (50, 0)

step :: State -> T.Text -> Maybe State
step (deg, pw) rot = case T.uncons rot of
  Just ('L', deg_str) -> Just (deg', pw + fromEnum (deg' == 0)) where deg' = (deg - readInt deg_str) `mod` dLimit
  Just ('R', deg_str) -> Just (deg', pw + fromEnum (deg' == 0)) where deg' = (deg + readInt deg_str) `mod` dLimit
  _ -> Nothing

part1 :: T.Text -> Maybe Int
part1 = fmap snd . foldM step start . T.lines

part2 :: T.Text -> Maybe Int
part2 _ = Nothing

solution :: IO ()
solution = solve day part1 part2

-- Tests
tests :: Spec
tests = describe "Day01" $ do
  it "solves part 1 with example 1" $ do
    input <- readExample day 1
    part1 input `shouldBe` Just 3

  it "solves part 2 with example 1" $ do
    input <- readExample day 1
    part2 input `shouldBe` Just 6
