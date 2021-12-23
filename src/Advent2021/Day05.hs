{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

module Advent2021.Day05
  ( part1
  , part2
  , parseInput
  , pointsCoveredByLine
  , countCoverage
  , Point(..)
  , Line(..)
  )
  where

import Data.Attoparsec.Text
import Data.Map.Strict qualified as M
import Data.Text qualified as T

part1 :: String -> String
part1 input = show . numPointsWithOverlap . filter lineIsHorizontalOrVertical $
  parseInput input

part2 :: String -> String
part2 input = show . numPointsWithOverlap $ parseInput input

data Point = Point Int Int
  deriving (Eq, Ord, Show)

data Line = Line Point Point
  deriving Show

pointP :: Parser Point
pointP = Point <$> decimal <* char ',' <*> decimal

lineP :: Parser Line
lineP = Line <$> pointP <* string " -> " <*> pointP

parseInput :: String -> [Line]
parseInput input =
  case parseOnly (many1 (lineP <* endOfLine)) $ T.pack input of
    Left err -> error $ "parse error: " ++ err
    Right res -> res

lineIsHorizontalOrVertical :: Line -> Bool
lineIsHorizontalOrVertical (Line (Point x1 y1) (Point x2 y2)) =
  x1 == x2 || y1 == y2

-- This produces a line of points as long as the lines are completely horizontal,
-- or vertical, as the problem states. Otherwise it would produce a rectangle.
pointsCoveredByLine :: Line -> [Point]
pointsCoveredByLine (Line (Point x1 y1) (Point x2 y2)) =
    [Point x y | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 .. max y1 y2]]

countCoverage :: [Line] -> M.Map Point Int
countCoverage ventLines =
    M.fromListWith (+) $ (, 1) <$> concatMap pointsCoveredByLine ventLines

numPointsWithOverlap :: [Line] -> Int
numPointsWithOverlap = M.size . M.filter (> 1) . countCoverage
