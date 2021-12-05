{-# LANGUAGE QuasiQuotes #-}

module Advent2021.Day02Spec (
  spec
) where

import Test.Hspec
import Advent2021.Day02
import Text.Heredoc

spec :: Spec
spec = describe "Day 02 tests" $ do
  it "follows the commands to the correct position" $ do
    part1 sample `shouldBe` "150"
  
  it "follows the commands to the correct position with aim" $ do
    part2 sample `shouldBe` "900"

sample :: [Char]
sample =
  [str|forward 5
      |down 5
      |forward 8
      |up 3
      |down 8
      |forward 2
      |]
