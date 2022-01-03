{-# LANGUAGE ImportQualifiedPost #-}

module Advent2021.Day09
  ( part1
  , part2
  )
  where

import Data.Maybe
import Data.Vector qualified as V

part1 :: String -> String
part1 input = show . sum . map succ . findLowPoints $ parseInput input

part2 :: String -> String
part2 = id

parseInput :: String -> V.Vector (V.Vector Int)
parseInput s =
  V.fromList $  V.fromList . map (read . (:"")) <$> lines s

lookup2 :: V.Vector (V.Vector a) -> Int -> Int -> Maybe a
lookup2 vv i j = do
  v <- vv V.!? i
  v V.!? j

findLowPoints :: V.Vector (V.Vector Int) -> [Int]
findLowPoints vv = V.ifoldr iterRow [] vv
  where
    iterRow :: Int -> V.Vector Int -> [Int] -> [Int]
    iterRow i v ps = V.ifoldr (iterCol i) ps v

    iterCol :: Int -> Int -> Int -> [Int] -> [Int]
    iterCol i j x ps = if isLowPoint x i j then x : ps else ps

    isLowPoint :: Int -> Int -> Int -> Bool
    isLowPoint x i j =
      let
        pN = lookup2 vv (pred i) j
        pW = lookup2 vv i (pred j)
        pE = lookup2 vv i (succ j)
        pS = lookup2 vv (succ i) j
        neighbors = catMaybes [pN, pW, pE, pS]
      in all (> x) neighbors
