module Day10 (solution, part1, part2, tests) where

import AoC.Lib (readNum)
import AoC.Template (Day (..), readExample, solve)
import Data.Bits
import qualified Data.List as L
import qualified Data.Text as T
import Test.Hspec
import Text.Regex.TDFA

day :: Day
day = Day 10

part1 :: T.Text -> Maybe Int
part1 input =
  let pattern = "\\[(.*)\\] \\((.*)\\) \\{(.*)\\}"
      toBits :: [Int] -> Int
      toBits = foldl (.|.) 0 . map bit
      fewests = do
        line <- lines . T.unpack $ input
        let (_, _, _, [lightsS, togglesS, _]) = line =~ pattern :: (String, String, String, [String])
        let lights = toBits . L.elemIndices '#' $ lightsS
        let toggles = map (toBits . map readNum . T.split (== ',')) . T.splitOn (T.pack ") (") . T.pack $ togglesS
        return . length . head . filter ((== lights) . foldl xor 0) . L.sortOn length $ L.subsequences toggles
   in Just $ sum fewests

part2 :: T.Text -> Maybe Int
part2 _ = Nothing

solution :: IO ()
solution = solve day part1 part2

-- Tests
tests :: Spec
tests = describe "Day10" $ do
  it "solves part 1 with example 1" $ do
    input <- readExample day 1
    part1 input `shouldBe` Just 7

  -- Uncomment to test with additional examples (create files like 10-2.txt, 10-3.txt, etc.)
  -- it "solves part 1 with example 2" $ do
  --   input <- readExample day 2
  --   part1 input `shouldBe` Just 0

  it "solves part 2 with example 1" $ do
    input <- readExample day 1
    part2 input `shouldBe` Just 0 -- TODO: Replace with expected value
