{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Advent2021.Day07.Continuous qualified as D7C
import Graphics.Vega.VegaLite qualified as VL
import Data.Aeson

main :: IO ()
main = do
  encodeFile "day07vis.json" $ VL.fromVL D7C.example1vis
