module Day07 (solution, part1, part2, tests) where

import AoC.Template (Day (..), readExample, solve)
import qualified Data.List as L
import qualified Data.Text as T
import Test.Hspec

day :: Day
day = Day 7

-- What an unsightly mess!

part :: T.Text -> Maybe (Int, Int)
part input = do
  let ls = T.lines input
  start <- T.findIndex (== 'S') $ head ls
  let (numSplits, beams) = descend 0 [(start, 1)] (tail . tail $ ls)
  return (numSplits, sum $ map snd beams)
  where
    descend numSplits beams [] = (numSplits, beams)
    descend numSplits beams (splittersT : rest) = descend (numSplits + n) beams' (tail rest)
      where
        n = length . filter ((== 2) . length) $ splitBeams
        beams' = map (foldl1 (\(x, y1) (_, y2) -> (x, y1 + y2))) . L.groupBy (\x y -> fst x == fst y) $ concat splitBeams
        splitBeams = forward (L.elemIndices '^' $ T.unpack splittersT) beams
        forward :: [Int] -> [(Int, Int)] -> [[(Int, Int)]]
        forward [] bs = map (: []) bs
        forward _ [] = []
        forward ss@(s : ss') bs@(b@(pos, mult) : bs') = case compare s pos of
          LT -> forward ss' bs
          EQ -> [(pos - 1, mult), (pos + 1, mult)] : forward ss' bs'
          GT -> [b] : forward ss bs'

part1 :: T.Text -> Maybe Int
part1 = fmap fst . part

part2 :: T.Text -> Maybe Int
part2 = fmap snd . part

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
