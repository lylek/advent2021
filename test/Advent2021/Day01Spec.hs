module Advent2021.Day01Spec (spec) where

import Test.Hspec
import Advent2021.Day01 (numLarger, numLargerWindows)

spec :: Spec
spec = describe "Day 01 tests" $ do
  let input = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

  it "finds the right number of increasing values" $ do
    numLarger input `shouldBe` 7
  
  it "finds the right number of increasing three-measurement sliding windows" $ do
    numLargerWindows input `shouldBe` 5

  
