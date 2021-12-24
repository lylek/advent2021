{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}

module Advent2021.Day06
  ( part1
  , part2
  ) where

import Data.List.Split
import Data.Vector.Unboxed qualified as V

part1 :: String -> String
part1 input = show $ numberOfFishAfterNDays 80 $ parseInput input

part2 :: String -> String
part2 input = show $ numberOfFishAfterNDays 256 $ parseInput input

parseInput :: String -> [Int]
parseInput input = map read $ splitOn "," $ head $ lines input

-- Part 2 became intractable to do by iterating over all fish timers, so we use the insight
-- that we only need to know how many timers there are with each value - a histogram of timers.
-- We then update those counts directly for each iteration.
--
ageAllFish :: V.Vector Int -> V.Vector Int
ageAllFish counts = V.generate 9 updateCount
  where
    updateCount 6 = counts V.! 7 + counts V.! 0
    updateCount 8 = counts V.! 0
    updateCount i = counts V.! succ i

numberOfFishAfterNDays :: Int -> [Int] -> Int
numberOfFishAfterNDays days timers = V.sum $ (!! days) $ iterate ageAllFish $ countTimers timers

countTimers :: [Int] -> V.Vector Int
countTimers timers = V.accum (+) (V.replicate 9 0) (map (, 1) timers)
