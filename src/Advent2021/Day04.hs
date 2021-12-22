{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Advent2021.Day04
  ( part1
  , part2
  , parseGame
  , getFirstWinningScore
  , getLastWinningScore
  )
  where

import Data.Either
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Attoparsec.Text
import Data.Text qualified as T
import Safe

part1 :: String -> String
part1 input =
  let (calledNums, boards) = parseGame input in
    case getFirstWinningScore calledNums boards of
      Nothing -> error "no winning score"
      Just score -> show score

parseGame :: String -> ([Int], [Board])
parseGame input =
  case parseOnly bingoP $ T.pack input of
    Left err -> error $ "parse error: " ++ err
    Right res -> res

getFirstWinningScore :: [Int] -> [Board] -> Maybe Int
getFirstWinningScore calledNums boards =
  headMay $ applyCalledNumbersToBoards calledNums boards

part2 :: String -> String
part2 input =
  let (calledNums, boards) = parseGame input in
    case getLastWinningScore calledNums boards of
      Nothing -> error "no winning score"
      Just score -> show score

getLastWinningScore :: [Int] -> [Board] -> Maybe Int
getLastWinningScore calledNums boards =
  lastMay $ applyCalledNumbersToBoards calledNums boards

data Position = Position
  { p_row :: Int
  , p_col :: Int
  }
  deriving Show

{-
For each board, we need to keep track of:
 * Locations of numbers
 * Unmarked numbers
 * For each row and column, count of marked numbers
-}

data Board = Board
  { b_unmarkedNumbers :: IntMap Position
  , b_markedNumbersPerRow :: IntMap Int
  , b_markedNumbersPerCol :: IntMap Int
  }
  deriving Show

bingoP :: Parser ([Int], [Board])
bingoP = (,) <$> calledNumsP <* endOfLine <*> (boardP `sepBy1` endOfLine) <* endOfInput

calledNumsP :: Parser [Int]
calledNumsP = decimal `sepBy` char ',' <* endOfLine

boardP :: Parser Board
boardP = boardRowsToBoard <$> boardRowsP

boardRowsToBoard :: [[Int]] -> Board
boardRowsToBoard = goRow 0 $ Board IM.empty IM.empty IM.empty
  where
    goRow _i board [] = board
    goRow i board (row : rows') = goCol i 0 board rows' row
    goCol i _j board rows' [] = goRow (succ i) board rows'
    goCol i j board@Board{..} rows' (n : ns') =
      let board' = board { b_unmarkedNumbers = IM.insert n (Position i j) b_unmarkedNumbers }
      in goCol i (succ j) board' rows' ns'

boardRowsP :: Parser [[Int]]
boardRowsP = many1 boardRowP

boardRowP :: Parser [Int]
boardRowP = many1 (many' (char ' ') *> decimal) <* endOfLine

applyCalledNumber :: Int -> Board -> Either Int Board
applyCalledNumber calledNum board = case IM.lookup calledNum (b_unmarkedNumbers board) of
  Nothing -> Right board
  Just (Position row col) ->
    let unmarkedNumbers' = IM.delete calledNum (b_unmarkedNumbers board)
        markedNumbersPerRow = b_markedNumbersPerRow board
        markedNumbersPerCol = b_markedNumbersPerCol board
        markedNumbersInRow' = succ $ IM.findWithDefault 0 row markedNumbersPerRow
        markedNumbersInCol' = succ $ IM.findWithDefault 0 col markedNumbersPerCol
        markedNumbersPerRow' = IM.insert row markedNumbersInRow' markedNumbersPerRow
        markedNumbersPerCol' = IM.insert col markedNumbersInCol' markedNumbersPerCol
        board' = Board unmarkedNumbers' markedNumbersPerRow' markedNumbersPerCol'
        win = markedNumbersInRow' == 5 || markedNumbersInCol' == 5
        sumOfUnmarked = sum $ IM.keys unmarkedNumbers'
    in if win then Left (calledNum * sumOfUnmarked) else Right board'

-- The problem description doesn't make clear how we should handle things if multiple boards
-- contain a called number. Probably this case won't occur in the test data. So we'll handle
-- it by returning all the winners, in the order the boards were defined.
--
applyCalledNumberToBoards :: Int -> [Board] -> ([Int], [Board])
applyCalledNumberToBoards calledNum boards =
  let boardsAfterNumber = applyCalledNumber calledNum <$> boards
  in partitionEithers boardsAfterNumber

-- This function will lazily produce winners, so Part 1 will stop after calculating the first winning score,
-- but Part 2 can continue on to calculate the last winning score.
--
applyCalledNumbersToBoards :: [Int] -> [Board] -> [Int]
applyCalledNumbersToBoards _calledNums [] = [] -- don't need to call any more numbers because everyone has won
applyCalledNumbersToBoards [] _boards = [] -- no more numbers to call, so there can be no more winners
applyCalledNumbersToBoards (calledNum : calledNums') boards =
  let (winners, boards') = applyCalledNumberToBoards calledNum boards
  in winners ++ applyCalledNumbersToBoards calledNums' boards'
