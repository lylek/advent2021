{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE QuasiQuotes #-}

module Advent2021.Day06Spec (spec) where

import Test.Hspec
import Text.Heredoc
import Advent2021.Day06

spec :: Spec
spec = describe "Day 06 tests" $ do

  it "calculates the right answer for part 1" $ do
    part1 sample `shouldBe` "5934"

sample :: String
sample =
  [str|3,4,3,1,2
  |]
