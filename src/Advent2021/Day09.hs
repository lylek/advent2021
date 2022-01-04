{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StrictData #-}

module Advent2021.Day09
  ( part1
  , part2
  )
  where

import Data.Set qualified as S
import Data.Maybe
import Data.Vector qualified as V
import Data.List ( sortOn )
import Data.Ord

part1 :: String -> String
part1 input = show . sum . map riskLevel . S.toList . findLowPoints $ parseInput input

part2 :: String -> String
part2 input =
  show
  . product
  . take 3
  . sortOn Down
  . map S.size
  . S.toList
  . findAllBasins
  $ parseInput input

type Grid a = V.Vector (V.Vector a)

parseInput :: String -> Grid Int
parseInput s =
  V.fromList $  V.fromList . map (read . (:"")) <$> lines s

lookupCoords :: Grid a -> Coords -> Maybe a
lookupCoords vv (Coords i j) = do
  v <- vv V.!? i
  v V.!? j

findLowPoints :: Grid Int -> S.Set Point
findLowPoints vv = S.fromList $ V.ifoldr iterRow [] vv
  where
    iterRow :: Int -> V.Vector Int -> [Point] -> [Point]
    iterRow i v ps = V.ifoldr (iterCol i) ps v

    iterCol :: Int -> Int -> Int -> [Point] -> [Point]
    iterCol i j x ps =
      if isLowPoint x i j then Point (Coords i j) x : ps else ps

    isLowPoint :: Int -> Int -> Int -> Bool
    isLowPoint x i j = all ((> x) . p_height) $ getNeighbors vv (Coords i j)

data Coords = Coords { c_row :: Int, c_col :: Int }
  deriving (Show, Eq, Ord)

getNeighbors :: Grid Int -> Coords -> [Point]
getNeighbors vv (Coords i j) =
  let
    cN = Coords (pred i) j
    cW = Coords i (pred j)
    cE = Coords i (succ j)
    cS = Coords (succ i) j
    pN = Point cN <$> lookupCoords vv cN
    pW = Point cW <$> lookupCoords vv cW
    pE = Point cE <$> lookupCoords vv cE
    pS = Point cS <$> lookupCoords vv cS
  in catMaybes [pN, pW, pE, pS]

data Point = Point
  { p_coords :: Coords
  , p_height :: Int
  }
  deriving (Show, Eq, Ord)

riskLevel :: Point -> Int
riskLevel = succ . p_height

{-
What criterion should we use to check whether to expand the basin?
1. The new point is no lower than the point we're visiting from
2. The new point has height less than 9

In fact, we have to check (2) anyway, even if we check (1).
If there are ridges in the basin, such that we can't cross the ridge at
some point, there would have to be another low point on the other side of the
ridge, and the ridge would be shared between the two basins, which the problem
statement says can't happen: each basin flows to a single low point. Thus
it must be sufficient to check (2).
-}

discoverBasin :: Grid Int -> Point -> S.Set Point
discoverBasin vv lp = go (S.singleton lp) [lp]
  where
    -- go accumulates a basin, and a stack of basin points for which we haven't
    -- yet visited the neighbors. When it runds out of points to visit, it returns
    -- the resulting basin.
    go :: S.Set Point -> [Point] -> S.Set Point
    go basin [] = basin
    go basin (p : ps') =
      let neighbors = getNeighbors vv (p_coords p)
          newBasinPoints =
            [n
            | n <- neighbors
            , p_height n /= 9
            , p_height n >= p_height p
            , not (S.member n basin)
            ]
      in go (S.fromList newBasinPoints `S.union` basin) (newBasinPoints ++ ps')

findAllBasins :: Grid Int -> S.Set (S.Set Point)
findAllBasins vv =
  let lowPoints = findLowPoints vv
  in S.map (discoverBasin vv) lowPoints
