{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE QuasiQuotes #-}

module Advent2021.Day07Spec (spec) where

import Test.Hspec
import Text.Heredoc
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (sample)
import Advent2021.Day07

spec :: Spec
spec = describe "Day 07 tests" $ do

  it "calculates the right answer for part 1" $ do
    part1 sample `shouldBe` "37"

  it "calculates the correct meanFloor when the mean is an integer" $ do
    meanFloor [1,11,6,7,18,5] `shouldBe` 8

  it "calculates the correct meanFloor of input values" $ do
    meanFloor (parseInput sample) `shouldBe` 4

  it "calculates the right answer for part 2" $ do
    part2 sample `shouldBe` "168"

  prop "the median minimizes the sum of distances" $ \ps y ->
    let ips = map getNonNegative ps
        m = median ips
    in
      sumOfDistances m ips <= sumOfDistances (m + y) ips

  prop "minimumCost minimizes the fuel cost" $ \ps y ->
    let ips = map getNonNegative ps
        (p, c) = minimumCost ips
     in c <= sumOfFuel (p+y) ips

sample :: String
sample =
  [str|16,1,2,0,4,2,7,1,2,14
  |]
