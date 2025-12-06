module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Printf (printf)
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
runDay day = do
  printf "Day %02d not yet implemented\n" day
  exitFailure
