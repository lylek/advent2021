{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}

module Advent2021.Day03
  ( part1
  , part2
  , parseInput
  , oxygenGeneratorRating
  , co2ScrubberRating
  , lifeSupportRating
  )
where

import Data.List (foldl', foldl1')

data BitCount = BitCount { bc_False :: Int, bc_True :: Int }
  deriving Show

type Bit = Bool

part1 :: String -> String
part1 input =
  let mcb = mostCommonBits . parseInput $ input
      lcb = map not mcb
      gammaRate = bitsToInt mcb
      epsilonRate = bitsToInt lcb
  in show $ gammaRate * epsilonRate

part2 :: String -> String
part2 = show . lifeSupportRating . parseInput

parseInput :: String -> [[Bit]]
parseInput = map parseBits . lines

parseBits :: String -> [Bit]
parseBits = map parseBit

parseBit :: Char -> Bit
parseBit '0' = False
parseBit '1' = True
parseBit c   = error $ "illegal bit: " ++ show c

mostCommonBits :: [[Bit]] -> [Bit]
mostCommonBits = map moreCommonBit . foldl1' addBitCounts . map countBits

countBits :: [Bit] -> [BitCount]
countBits = map countBit

countBit :: Bit -> BitCount
countBit b = if b then BitCount 0 1 else BitCount 1 0

addBitCounts :: [BitCount] -> [BitCount] -> [BitCount]
addBitCounts = zipWith addBitCount

addBitCount :: BitCount -> BitCount -> BitCount
addBitCount (BitCount i0 i1) (BitCount j0 j1) = BitCount (i0 + j0) (i1 + j1)

mostCommonBit :: [BitCount] -> Bit
mostCommonBit = moreCommonBit . foldl' addBitCount (BitCount 0 0)

moreCommonBit :: BitCount -> Bit
moreCommonBit (BitCount i0 i1) = i0 <= i1

bitsToInt :: [Bit] -> Int
bitsToInt bs = go (0, bs)
  where go (!n, []) = n
        go (!n, True : bs') = go (n * 2 + 1, bs')
        go (!n, False : bs') = go (n * 2, bs')

lifeSupportRating :: [[Bit]] -> Int
lifeSupportRating = (*) <$> oxygenGeneratorRating <*> co2ScrubberRating

narrowByColumnsUntilOneValueLeft :: ([([Bit], [Bit])] -> [([Bit], [Bit])]) -> [([Bit], [Bit])] -> [Bit]
narrowByColumnsUntilOneValueLeft narrower = go
  where
    go :: [([Bit], [Bit])] -> [Bit]
    go bitVecsWithTails =
      let narrowedBitVecsWithTails = narrower bitVecsWithTails
      in
        case narrowedBitVecsWithTails of
          [] -> error "ran out of bit vectors"
          [(bv, _)] -> bv
          _ -> go $ map nextTail narrowedBitVecsWithTails

    nextTail :: ([Bit], [Bit]) -> ([Bit], [Bit])
    nextTail (_bv, []) = error "ran out of columns"
    nextTail (bv, _ : tl') = (bv, tl')

pairBitVecsWithTails :: [[Bit]] -> [([Bit], [Bit])]
pairBitVecsWithTails = map (\x -> (x, x))

oxygenGeneratorRating :: [[Bit]] -> Int
oxygenGeneratorRating = bitsToInt . narrowByColumnsUntilOneValueLeft narrowByMostCommonFirstBit . pairBitVecsWithTails

co2ScrubberRating :: [[Bit]] -> Int
co2ScrubberRating = bitsToInt . narrowByColumnsUntilOneValueLeft narrowByLeastCommonFirstBit . pairBitVecsWithTails

narrowByFirstBit :: (Bit -> Bool) -> [([Bit], [Bit])] -> [([Bit], [Bit])]
narrowByFirstBit f bitVecsWithTails =
  let keepBitVec (_, []) = error "impossible: should not have run out of columns"
      keepBitVec (_, fb : _) = f fb
  in filter keepBitVec bitVecsWithTails

narrowByMostCommonFirstBit :: [([Bit], [Bit])] -> [([Bit], [Bit])]
narrowByMostCommonFirstBit bitVecsWithTails =
  let mcb = mostCommonFirstBit $ map snd bitVecsWithTails
  in narrowByFirstBit (== mcb) bitVecsWithTails

narrowByLeastCommonFirstBit :: [([Bit], [Bit])] -> [([Bit], [Bit])]
narrowByLeastCommonFirstBit bitVecsWithTails =
  let lcb = not $ mostCommonFirstBit $ map snd bitVecsWithTails
  in narrowByFirstBit (== lcb) bitVecsWithTails

mostCommonFirstBit :: [[Bit]] -> Bit
mostCommonFirstBit bvs = mostCommonBit . countBits $ map head bvs
