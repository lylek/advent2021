{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Advent2021.Day08
  ( part1
  , part2
  ) where

import Data.Attoparsec.Text
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import GHC.Stack
import Safe

part1 :: String -> String
part1 input = show . sum . fmap countOutputDigitsWithUniqueNumberOfSegments $
  parseInput input

part2 :: HasCallStack => String -> String
part2 input = show . sum $
  (\display -> decodeOutput (solveWirings (d_patterns display)) (d_outputs display)) <$>
    parseInput input

data Display = Display
  { d_patterns :: S.Set (S.Set Char)
  , d_outputs :: [S.Set Char]
  }
  deriving (Show, Eq, Ord)

parseInput :: String -> [Display]
parseInput input =
  case parseOnly (many1 (displayP <* endOfLine) <* endOfInput) $ T.pack input of
    Left err -> error $ "parse error: " ++ err
    Right res -> res

displayP :: Parser Display
displayP = Display <$> patternSetP <* string " | " <*> patternListP

patternP :: Parser (S.Set Char)
patternP = S.fromList <$> many1 (satisfy ((&&) <$> (>= 'a') <*> (<= 'g')))

patternListP :: Parser [S.Set Char]
patternListP = sepBy1 patternP " "

patternSetP :: Parser (S.Set (S.Set Char))
patternSetP = S.fromList <$> patternListP

countOutputDigitsWithUniqueNumberOfSegments :: Display -> Int
countOutputDigitsWithUniqueNumberOfSegments display =
  length $ filter (\o -> S.member (S.size o) uniqueNumbersOfSegments) (d_outputs display)
  where
    uniqueNumbersOfSegments = S.fromList [2,3,4,7]

solveWirings :: HasCallStack => S.Set (S.Set Char) -> M.Map (S.Set Char) Int
solveWirings patterns =
  let
      -- There are four digits we can identify because their patterns have a unique
      -- number of segments.
      patternsByNumSegments = M.fromListWith S.union
        [(S.size p, S.singleton p) | p <- S.toList patterns]
      d1 = getSingleton $ patternsByNumSegments ! 2
      d7 = getSingleton $ patternsByNumSegments ! 3
      d4 = getSingleton $ patternsByNumSegments ! 4
      d8 = getSingleton $ patternsByNumSegments ! 7

      -- The 'a' segment is the only difference between digits 7 and 1
      inA = getSingleton $ d7 S.\\ d1

      -- There are three input segments that appear in a unique number of digits.
      numAppearancesByOutputSegment =
        M.unionsWith (+)
          . fmap (M.fromListWith (+) . fmap (, 1 :: Int) . S.toList)
          $ S.toList patterns
      outputSegmentsByNumAppearances = M.fromListWith S.union $
        (\(k, v) -> (v, S.singleton k)) <$> M.toList numAppearancesByOutputSegment
      inE = getSingleton $ outputSegmentsByNumAppearances ! 4
      inB = getSingleton $ outputSegmentsByNumAppearances ! 6
      inF = getSingleton $ outputSegmentsByNumAppearances ! 9

      -- Now that we've identified the output corresponding to the 'e' input,
      -- we know that the digit 9 is missing only that segment.
      allSegments = S.fromList "abcdefg"
      d9 = S.delete inE allSegments

      -- Since the digit 1 consists of input segments 'f' and 'c', and we have
      -- identified the output segment corresponding to to 'f', we can remove
      -- the output wired to 'f' to get the output wired to 'c'.
      inC = getSingleton $ S.delete inF d1

      -- Since we've identified the digit 9, there are two other digits with 6
      -- segments: 0 and 6. The one with the segment corresponding to input 'c'
      -- is digit 0, and the other is the digit 6.
      d0and6 = S.delete d9 $ patternsByNumSegments ! 6
      (d0s, d6s) = S.partition (S.member inC) d0and6
      d0 = getSingleton d0s
      d6 = getSingleton d6s

      -- Since we've identified the digit 0, and it's missing only input segment
      -- 'd', we can identify the output segment corresponding to 'd'.
      inD = getSingleton $ allSegments S.\\ d0

      -- The only remaining unidentified wiring is for the input segment 'g'.
      inG = getSingleton $ allSegments S.\\ S.fromList [inA, inB, inC, inD, inE, inF]

      -- Now we know all the wirings, so we can reconstruct the remaining three digits
      -- from those.
      d2 = S.fromList [inA, inC, inD, inE, inG]
      d3 = S.fromList [inA, inC, inD, inF, inG]
      d5 = S.fromList [inA, inB, inD, inF, inG]

   in M.fromList
        [ (d0, 0)
        , (d1, 1)
        , (d2, 2)
        , (d3, 3)
        , (d4, 4)
        , (d5, 5)
        , (d6, 6)
        , (d7, 7)
        , (d8, 8)
        , (d9, 9)
        ]

getSingleton :: HasCallStack => S.Set a -> a
getSingleton = headNote "expected singleton set" . S.toList

decodeOutput :: HasCallStack => M.Map (S.Set Char) Int -> [S.Set Char] -> Int
decodeOutput wirings = fromDigits . fmap (wirings !)

(!) :: (HasCallStack, Ord k, Show k) => M.Map k v -> k -> v
(!) m k = fromJustNote ("failed to find key " ++ show k ++ " in map") $
  M.lookup k m

fromDigits :: [Int] -> Int
fromDigits = go 0
  where
    go !n [] = n
    go !n (d:ds') = go (n*10 + d) ds'
