module Day03 (solution, part1, part2, tests) where

import AoC.Template (Day (..), readExample, solve)
import Data.List (elemIndex)
import qualified Data.Text as T
import Test.Hspec

day :: Day
day = Day 3

maxJoltage :: String -> Maybe Int
maxJoltage s = do
  let mx = maximum s
  argmax <- elemIndex mx s
  let (c, d) = if argmax == length s - 1 then (maximum $ init s, mx) else (mx, maximum $ drop (argmax + 1) s)
  return $ read [c, d]

part1 :: T.Text -> Maybe Int
part1 = fmap sum . mapM (maxJoltage . T.unpack) . T.lines

part2 :: T.Text -> Maybe Int
part2 _ = Nothing

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
