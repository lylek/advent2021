{-# LANGUAGE QuasiQuotes #-}

module Advent2021.Day10Spec (spec) where

import Test.Hspec
import Advent2021.Day10 (part1, part2)
import Text.Heredoc

spec :: Spec
spec = describe "Day 10 tests" $ do

  it "calculates the right answer for part 1" $ do
    part1 sample `shouldBe` "26397"

sample :: String
sample =
    [str|[({(<(())[]>[[{[]{<()<>>
        |[(()[<>])]({[<{<<[]>>(
        |{([(<{}[<>[]}>{[]{[(<()>
        |(((({<>}<{<{<>}{[]{[]{}
        |[[<[([]))<([[{}[[()]]]
        |[{[{({}]{}}([{[{{{}}([]
        |{<[[]]>}<{[{[{[]{()[[[]
        |[<(<(<(<{}))><([]([]()
        |<{([([[(<>()){}]>(<<{{
        |<{([{{}}[<[[[<>{}]]]>[]]
        |]
