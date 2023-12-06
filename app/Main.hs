module Main (main) where

import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text.IO as TIO (readFile)
import qualified Day1 (solve1, solve2)
import qualified Day2 (solve1, solve2)
import qualified Day3 (solve1, solve2)
import qualified Day4 (solve1, solve2)
import qualified Day5 (solve1, solve2)
import qualified Day6 (solve1, solve2)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

getInput :: Int -> IO Text
getInput i = TIO.readFile $ "data/" ++ show i ++ ".txt"

getSolvers :: Int -> Maybe (Text -> Int, Text -> Int)
getSolvers i
  | i == 1 = Just (Day1.solve1, Day1.solve2)
  | i == 2 = Just (Day2.solve1, Day2.solve2)
  | i == 3 = Just (Day3.solve1, Day3.solve2)
  | i == 4 = Just (Day4.solve1, Day4.solve2)
  | i == 5 = Just (Day5.solve1, Day5.solve2)
  | i == 6 = Just (Day6.solve1, Day6.solve2)
  | otherwise = Nothing

run :: Int -> IO ()
run i = do
  fileContent <- getInput i
  let solver = fromJust $ getSolvers i
  putStrLn $ "Day " ++ show i
  putStrLn $ "Part 1: " ++ show (fst solver fileContent)
  putStrLn $ "Part 2: " ++ show (snd solver fileContent)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> run (read file :: Int)
    _ -> do
      name <- getProgName
      hPutStrLn stderr $ "usage: " ++ name ++ " <string>"
      exitFailure
