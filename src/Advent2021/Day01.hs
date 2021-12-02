module Advent2021.Day01
 ( part1
 , part2
 , numLarger
 , numLargerWindows
 )
where

part1 :: String -> String
part1 input = show $ numLarger $ read <$> lines input

part2 :: String -> String
part2 input = show $ numLargerWindows $ read <$> lines input

numLarger :: [Int] -> Int
numLarger ns = length $ filter id $ zipWith (<) ns (tail ns)

numLargerWindows :: [Int] -> Int
numLargerWindows ns = numLarger $ zipWith3 (\n1 n2 n3 -> n1 + n2 + n3) ns (drop 1 ns) (drop 2 ns)
