{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import System.Environment
import Advent2021.Day01 qualified as Day01
import Advent2021.Day02 qualified as Day02
import Advent2021.Day03 qualified as Day03
import Advent2021.Day04 qualified as Day04
import Advent2021.Day05 qualified as Day05
import Advent2021.Day06 qualified as Day06

main :: IO ()
main = do
  args <- getArgs
  let (day, part, filename) = case args of
        [arg1, arg2, arg3] -> (read arg1, read arg2, arg3) :: (Int, Int, String)
        _ -> error "Usage: advent2021 [day] [part] [input file]"
  let func = case day of
        1 -> case part of
          1 -> Day01.part1
          2 -> Day01.part2
          _ -> error $ "unknown part " ++ show part
        2 -> case part of
          1 -> Day02.part1
          2 -> Day02.part2
          _ -> error $ "unknown part " ++ show part
        3 -> case part of
          1 -> Day03.part1
          2 -> Day03.part2
          _ -> error $ "unknown part " ++ show part
        4 -> case part of
          1 -> Day04.part1
          2 -> Day04.part2
          _ -> error $ "unknown part " ++ show part
        5 -> case part of
          1 -> Day05.part1
          2 -> Day05.part2
          _ -> error $ "unknown part " ++ show part
        6 -> case part of
          1 -> Day06.part1
          2 -> undefined -- Day06.part2
          _ -> error $ "unknown part " ++ show part
        _ -> error $ "unknown day " ++ show day
  inputStr <- readFile filename
  putStrLn $ func inputStr
  pure ()
