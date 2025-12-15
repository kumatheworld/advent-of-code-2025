{-# LANGUAGE TupleSections #-}

module Day10 (solution, part1, part2, tests) where

import AoC.Template (Day (..), readExample, solve)
import Data.Bits
import qualified Data.List as L
import Data.List.Split
import qualified Data.Map.Lazy as Map
import Data.Scientific
import Data.String
import qualified Data.Text as T
import qualified Numeric.Optimization.MIP as MIP
import qualified Numeric.Optimization.MIP.Solver as MIPS
import System.IO.Unsafe
import Test.Hspec
import Text.Regex.TDFA

day :: Day
day = Day 10

part :: T.Text -> [(Int, [Int], [Int])]
part input = do
  line <- lines . T.unpack $ input
  let (lightsS, togglesS, joltageS) = readTriple line "\\[(.*)\\] \\((.*)\\) \\{(.*)\\}"
  let lights = toBits . L.elemIndices '#' $ lightsS
  let toggles = map (toBits . map read . splitOn ",") . splitOn ") (" $ togglesS
  let joltage = map read $ splitOn "," joltageS
  return (lights, toggles, joltage)
  where
    toBits = foldl (.|.) 0 . map bit
    readTriple :: String -> String -> (String, String, String)
    readTriple line pattern =
      let (_, _, _, submatches) = line =~ pattern :: (String, String, String, [String])
       in case submatches of
            [l, t, j] -> (l, t, j)
            _ -> undefined

part1 :: T.Text -> Maybe Int
part1 input =
  Just . sum $ do
    (lights, toggles, _) <- part input
    return . length . head . filter ((== lights) . foldl xor 0) . L.sortOn length $ L.subsequences toggles

part2 :: T.Text -> Maybe Int
part2 input =
  fmap sum . sequence $ do
    (_, toggles, joltage) <- part input
    let varsS = map (fromString . ('x' :) . show) toggles
        vars = map MIP.varExpr varsS
        prob =
          MIP.def
            { MIP.objectiveFunction =
                MIP.def
                  { MIP.objDir = MIP.OptMin,
                    MIP.objExpr = sum vars
                  },
              MIP.constraints =
                zipWith
                  ( \i j ->
                      sum (map snd . filter ((`testBit` i) . fst) $ zip toggles vars) MIP..==. j
                  )
                  [0 ..]
                  (map fromIntegral joltage),
              MIP.varDomains =
                Map.fromList $
                  map (,(MIP.IntegerVariable, (0, MIP.PosInf))) varsS
            }
        sol = unsafePerformIO $ MIPS.solve MIPS.cbc MIP.def prob -- should do IO outside of part2
     in return $ toBoundedInteger =<< MIP.solObjectiveValue sol

-- My initial attempt, probably not working in the intended way either
-- import Control.Monad
-- import Control.Monad.ST
-- import qualified Data.Vector as V
-- import qualified Data.Vector.Unboxed as VU
-- import qualified Data.Vector.Unboxed.Mutable as VUM
-- part2 :: T.Text -> Maybe Int
-- part2 input =
--   fmap sum . sequence $ do
--     (_, toggles, joltage) <- part input
--     let nv = bit (length joltage)
--     let ids = L.sortOn popCount [1 .. nv - 1]
--     -- make sure the endian is correct
--     let relTogIds = V.fromList $ map (\j -> map (xor j) $ filter ((/= 0) . (j .&.)) toggles) [0 .. nv - 1]
--     let run = runST $ do
--           v <- VUM.replicate nv maxBound
--           VUM.write v (nv - 1) 0
--           forM_
--             [1 .. maximum joltage]
--             ( \i -> do
--                 VUM.read v (nv - 1) >>= VUM.write v 0
--                 forM_
--                   ids
--                   ( \j -> do
--                       prev <- mapM (VUM.read v) (relTogIds V.! j)
--                       VUM.write
--                         v
--                         j
--                         ( if any ((i >) . snd) . filter (testBit j . fst) $ zip [length joltage - 1 .. 0] joltage
--                             then maxBound
--                             else
--                               let decents = filter (/= maxBound) prev
--                                in if null decents then maxBound else minimum decents + 1
--                         )
--                   )
--             )
--           VU.freeze v
--     return . Just . VU.last . VU.filter (/= maxBound) $ run

solution :: IO ()
solution = solve day part1 part2

-- Tests
tests :: Spec
tests = describe "Day10" $ do
  it "solves part 1 with example 1" $ do
    input <- readExample day 1
    part1 input `shouldBe` Just 7

  it "solves part 2 with example 1" $ do
    input <- readExample day 1
    part2 input `shouldBe` Just 33
