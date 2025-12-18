module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Printf (printf)
import qualified Day12
import qualified Day11
import qualified Day10
import qualified Day09
import qualified Day08
import qualified Day07
import qualified Day06
import qualified Day05
import qualified Day04
import qualified Day03
import qualified Day02
import qualified Day01

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dayStr] -> do
      let day = read dayStr :: Int
      runDay day
    _ -> do
      putStrLn "Usage: cabal run day <day>"
      exitFailure

runDay :: Int -> IO ()
runDay 1 = Day01.solution
runDay 2 = Day02.solution
runDay 3 = Day03.solution
runDay 4 = Day04.solution
runDay 5 = Day05.solution
runDay 6 = Day06.solution
runDay 7 = Day07.solution
runDay 8 = Day08.solution
runDay 9 = Day09.solution
runDay 10 = Day10.solution
runDay 11 = Day11.solution
runDay 12 = Day12.solution
runDay day = do
  printf "Day %02d not yet implemented\n" day
  exitFailure
