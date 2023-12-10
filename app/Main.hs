module Main (main) where

import Data.Text (Text)
import qualified Data.Text.IO as TIO (readFile)
import qualified Day1 (solve1, solve2)
import qualified Day10 (solve1, solve2)
import qualified Day2 (solve1, solve2)
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

maxDay = 9

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
    [num, file] -> run (read num :: Int) (Just file)
    _ -> do
      name <- getProgName
      hPutStrLn stderr $ "usage: " ++ name ++ " <string>"
      exitFailure
