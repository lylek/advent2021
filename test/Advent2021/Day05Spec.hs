{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE QuasiQuotes #-}

module Advent2021.Day05Spec (spec) where

import Test.Hspec
import Text.Heredoc
import Advent2021.Day05
import Data.Map.Strict qualified as M
import Data.Set qualified as S

spec :: Spec
spec = describe "Day 05 tests" $ do

  it "calculates the right points for an increasing horizontal line" $
    S.fromList (pointsCoveredByLine (Line (Point 0 9) (Point 2 9)))
      `shouldBe` S.fromList [Point 0 9, Point 1 9, Point 2 9]

  it "calculates the right points for a decreasing horizontal line" $
    S.fromList (pointsCoveredByLine (Line (Point 2 9) (Point 0 9)))
      `shouldBe` S.fromList [Point 2 9, Point 1 9, Point 0 9]

  it "calculates the right points for an increasing vertical line" $
    S.fromList (pointsCoveredByLine (Line (Point 3 5) (Point 3 8)))
      `shouldBe` S.fromList [Point 3 5, Point 3 6, Point 3 7, Point 3 8]

  it "calculates the right points for a decreasing vertical line" $
    S.fromList (pointsCoveredByLine (Line (Point 3 8) (Point 3 5)))
      `shouldBe` S.fromList [Point 3 8, Point 3 7, Point 3 6, Point 3 5]

  it "calculates the right coverage counts for a plus-sign shape" $
    countCoverage [Line (Point 2 1) (Point 2 3), Line (Point 1 2) (Point 3 2)]
      `shouldBe` M.fromList [(Point 2 1, 1), (Point 2 2, 2), (Point 2 3, 1), (Point 1 2, 1), (Point 3 2, 1)]

  it "calculates the right answer for part 1" $ do
    part1 sample `shouldBe` "5"

sample :: String
sample =
  [str|0,9 -> 5,9
      |8,0 -> 0,8
      |9,4 -> 3,4
      |2,2 -> 2,1
      |7,0 -> 7,4
      |6,4 -> 2,0
      |0,9 -> 2,9
      |3,4 -> 1,4
      |0,0 -> 8,8
      |5,5 -> 8,2
      |]
