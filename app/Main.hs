module Main (main) where

import Data.Text (Text)
import qualified Data.Text.IO as TIO (readFile)
import qualified Day1 (solve1, solve2)
import qualified Day10 (solve1, solve2)
import qualified Day11 (solve1, solve2)
import qualified Day12 (solve1, solve2)
import qualified Day13 (solve1, solve2)
import qualified Day14 (solve1, solve2)
import qualified Day15 (solve1, solve2)
import qualified Day16 (solve1, solve2)
import qualified Day17 (solve1, solve2)
import qualified Day18 (solve1, solve2)
import qualified Day19 (solve1, solve2)
import qualified Day2 (solve1, solve2)
import qualified Day20 (solve1, solve2)
import qualified Day21 (solve1, solve2)
import qualified Day22 (solve1, solve2)
import qualified Day23 (printGraph, solve1, solve2)
import qualified Day24 (solve1, solve2)
import qualified Day25 (solve1, solve2)
import qualified Day3 (solve1, solve2)
import qualified Day4 (solve1, solve2)
import qualified Day5 (solve1, solve2)
import qualified Day6 (solve1, solve2)
import qualified Day7 (solve1, solve2)
import qualified Day8 (solve1, solve2)
import qualified Day9 (solve1, solve2)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

maxDay = 25

getSolvers :: Int -> (Text -> Int, Text -> Int)
getSolvers i
  | i == 1 = (Day1.solve1, Day1.solve2)
  | i == 2 = (Day2.solve1, Day2.solve2)
  | i == 3 = (Day3.solve1, Day3.solve2)
  | i == 4 = (Day4.solve1, Day4.solve2)
  | i == 5 = (Day5.solve1, Day5.solve2)
  | i == 6 = (Day6.solve1, Day6.solve2)
  | i == 7 = (Day7.solve1, Day7.solve2)
  | i == 8 = (Day8.solve1, Day8.solve2)
  | i == 9 = (Day9.solve1, Day9.solve2)
  | i == 10 = (Day10.solve1, Day10.solve2)
  | i == 11 = (Day11.solve1, Day11.solve2)
  | i == 12 = (Day12.solve1, Day12.solve2)
  | i == 13 = (Day13.solve1, Day13.solve2)
  | i == 14 = (Day14.solve1, Day14.solve2)
  | i == 15 = (Day15.solve1, Day15.solve2)
  | i == 16 = (Day16.solve1, Day16.solve2)
  | i == 17 = (Day17.solve1, Day17.solve2)
  | i == 18 = (Day18.solve1, Day18.solve2)
  | i == 19 = (Day19.solve1, Day19.solve2)
  | i == 20 = (Day20.solve1, Day20.solve2)
  | i == 21 = (Day21.solve1, Day21.solve2)
  | i == 22 = (Day22.solve1, Day22.solve2)
  | i == 23 = (Day23.solve1, Day23.solve2)
  | i == 24 = (Day24.solve1, Day24.solve2)
  | i == 25 = (Day25.solve1, Day25.solve2)
  | otherwise = error $ "Unknown day " ++ show i

runWithFile :: Int -> String -> IO ()
runWithFile i file = do
  fileContent <- TIO.readFile file
  let (solve1, solve2) = getSolvers i
  putStrLn $ "Day " ++ show i ++ " (" ++ file ++ ")"
  putStrLn $ "Part 1: " ++ show (solve1 fileContent)
  putStrLn $ "Part 2: " ++ show (solve2 fileContent)

run :: Int -> Maybe String -> IO ()
run i Nothing = runWithFile i $ "data/" ++ show i ++ ".txt"
run i (Just file) = runWithFile i file

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["all"] -> mapM_ (`run` Nothing) [1 .. maxDay]
    [num] -> run (read num :: Int) Nothing
    ["23graph", file] -> do
      fileContent <- TIO.readFile file
      Day23.printGraph fileContent
    [num, file] -> run (read num :: Int) (Just file)
    _ -> do
      name <- getProgName
      hPutStrLn stderr $ "usage: " ++ name ++ " <string>"
      exitFailure
