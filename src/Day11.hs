module Day11 (solution, part1, part2, tests) where

import AoC.Lib (splitToPair)
import AoC.Template (Day (..), readExample, solve)
import Control.Monad
import Control.Monad.ST
import qualified Data.Array as A
import Data.Graph
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Vector.Unboxed.Mutable as VUM
import Test.Hspec

day :: Day
day = Day 11

part :: String -> String -> T.Text -> Maybe Int
part source target input = do
  let extendedInput = T.pack "out: " : T.lines input
      (graph, _, vertexFromKey) = graphFromEdges $ map ((\(s, ts) -> (s, s, T.words ts)) . splitToPair ": ") extendedInput
      vs = topSort graph
      findV name = do
        v <- vertexFromKey (T.pack name)
        vid <- v `L.elemIndex` vs
        return (v, vid)
      reversedGraph = transposeG graph
  (s, sV) <- findV source
  (t, tV) <- findV target
  return $ runST $ do
    nPaths <- VUM.replicate (length vs) 0
    VUM.write nPaths s 1
    forM_
      (drop (sV + 1) $ take (tV + 1) vs)
      ( \v -> mapM (VUM.read nPaths) (reversedGraph A.! v) >>= VUM.write nPaths v . sum
      )
    VUM.read nPaths t

part1 :: T.Text -> Maybe Int
part1 = part "you" "out"

-- Some repetitions here but who cares
part2 :: T.Text -> Maybe Int
part2 input = product <$> sequence [part "svr" "fft" input, part "fft" "dac" input, part "dac" "out" input]

solution :: IO ()
solution = solve day part1 part2

-- Tests
tests :: Spec
tests = describe "Day11" $ do
  it "solves part 1 with example 1" $ do
    input <- readExample day 1
    part1 input `shouldBe` Just 5

  it "solves part 2 with example 2" $ do
    input <- readExample day 2
    part2 input `shouldBe` Just 2
