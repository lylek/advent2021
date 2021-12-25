{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE QuasiQuotes #-}

module Advent2021.Day07Spec (spec) where

import Test.Hspec
import Text.Heredoc
import Advent2021.Day07

spec :: Spec
spec = describe "Day 07 tests" $ do

  it "calculates the right answer for part 1" $ do
    part1 sample `shouldBe` "37"

sample :: String
sample =
  [str|16,1,2,0,4,2,7,1,2,14
  |]
