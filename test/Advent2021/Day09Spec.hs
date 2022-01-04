{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE QuasiQuotes #-}

module Advent2021.Day09Spec (spec) where

import Test.Hspec
import Text.Heredoc
import Advent2021.Day09 (part1, part2)

spec :: Spec
spec = describe "Day 09 tests" $ do

  it "calculates the right answer for part 1" $ do
    part1 sample `shouldBe` "15"

  it "calculates the right answer for part 2" $ do
    part2 sample `shouldBe` "1134"

sample :: String
sample =
  [str|2199943210
      |3987894921
      |9856789892
      |8767896789
      |9899965678
      |]
