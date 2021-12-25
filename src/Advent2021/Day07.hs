module Advent2021.Day07
  ( part1
  , part2
  ) where

import Data.List ( sort )
import Data.List.Split ( splitOn )

part1 :: String -> String
part1 input =
  let positions = parseInput input
  in show $ sumOfDistances (median positions) positions

part2 :: String -> String
part2 = id

parseInput :: String -> [Int]
parseInput input = map read $ splitOn "," $ head $ lines input

-- If there are an odd number of elements, finds the only median.
-- If there are an even number of elements, finds the highest median.
-- Any median will yield the same sum of distances.
median :: [Int] -> Int
median [] = error "median of empty list"
median xs = sort xs !! (length xs `div` 2)

sumOfDistances :: Int -> [Int] -> Int
sumOfDistances y xs = sum (abs . subtract y <$> xs)
