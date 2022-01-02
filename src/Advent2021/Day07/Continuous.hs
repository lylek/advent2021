{-# LANGUAGE OverloadedStrings #-}

module Advent2021.Day07.Continuous
  ( mean
  , sumOfFuel
  , example1
  , example1vis
  ) where

import Graphics.Vega.VegaLite

mean :: [Double] -> Double
mean [] = error "mean of empty list"
mean xs = sum xs / fromIntegral (length xs)

sumOfFuel :: Double -> [Double] -> Double
sumOfFuel y xs = sum [((x-y) * (x-y+signum(x-y))) / 2 | x <- xs]

example1 :: [Double]
example1 = [0,2,6,10,15]

example1vis :: VegaLite
example1vis =
  let dat = dataFromRows [] .
        foldl1 (.) [dataRow [("Position", Number x), ("Cost", Number $ sumOfFuel x example1)] | x <- [6, 6.1 .. 7]]
      enc = encoding
        . position X [PName "Position", PmType Quantitative, PAxis [AxTickCount 15]]
        . position Y [PName "Cost", PmType Quantitative, PAxis [AxTickCount 12], PScale [SDomain (DNumbers [80, 90])]]
  in toVegaLite
      [ width 500
      , height 500
      , dat []
      , mark Line [MInterpolate Basis]
      , enc []
      ]

-- 6.5 has a lower cost than 6.6, even though 6.6 is the mean
