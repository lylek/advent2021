module Advent2021.Day06
  ( part1
  ) where

import Data.List.Split

part1 :: String -> String
part1 input = show $ length $ (!! 80) $ iterate ageAllFish $ parseInput input

parseInput :: String -> [Int]
parseInput input = map read $ splitOn "," $ head $ lines input

ageOneFish :: Int -> [Int]
ageOneFish 0 = [6, 8]
ageOneFish timer = [pred timer]

ageAllFish :: [Int] -> [Int]
ageAllFish = concatMap ageOneFish

--part1 :: String -> String
--part1 = show . length . (!! 80) . iterate (concatMap (\n -> if n == (0 :: Int) then [6,8] else [n-1])) . map read . splitOn "," . head . lines
