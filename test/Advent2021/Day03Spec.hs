{-# LANGUAGE QuasiQuotes #-}

module Advent2021.Day03Spec (spec) where

import Test.Hspec
import Text.Heredoc
import Advent2021.Day03

spec :: Spec
spec = describe "Day 03 tests" $ do
  it "calculates the right power consumption" $ do
    part1 sample `shouldBe` "198"
  
  it "calculates the right oxygen generator rating" $ do
    (oxygenGeneratorRating . parseInput $ sample) `shouldBe` 23

  it "calculates the right CO2 scrubber rating" $ do
    (co2ScrubberRating . parseInput $ sample) `shouldBe` 10
  
  it "calculates the right life support rating" $ do
    (lifeSupportRating . parseInput $ sample) `shouldBe` 230
  
  it "calculates the right result for part 2" $ do
    part2 sample `shouldBe` "230"

sample :: String
sample =
  [str|00100
      |11110
      |10110
      |10111
      |10101
      |01111
      |00111
      |11100
      |10000
      |11001
      |00010
      |01010
      |]