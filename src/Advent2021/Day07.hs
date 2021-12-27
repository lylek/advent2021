module Advent2021.Day07
  ( part1
  , part2
  , parseInput
  , median
  , meanFloor
  , minimumCost
  , sumOfDistances
  , sumOfFuel
  ) where

import Data.List ( sort )
import Data.List.Split ( splitOn )

part1 :: String -> String
part1 input =
  let positions = parseInput input
  in show $ sumOfDistances (median positions) positions

part2 :: String -> String
part2 input =
  let positions = parseInput input
  in show $ snd $ minimumCost positions

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

meanFloor :: [Int] -> Int
meanFloor [] = error "meanFloor of empty list"
meanFloor xs = sum xs `div` length xs

-- The mean minimizes the cost, but the mean may fall between two integer
-- positions. In that case, the position to the left or the right of the
-- mean could have the lowest cost.
--
minimumCost :: [Int] -> (Int, Int)
minimumCost positions =
  let mf = meanFloor positions
      cost1 = sumOfFuel mf positions
      cost2 = sumOfFuel (succ mf) positions
   in if cost1 <= cost2 then (mf, cost1) else (succ mf, cost2)

sumOfFuel :: Int -> [Int] -> Int
sumOfFuel y xs = sum [((x-y) * (x-y+signum(x-y))) `div` 2 | x <- xs]