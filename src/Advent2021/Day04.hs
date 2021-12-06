{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Advent2021.Day04
  ( part1
  , parseGame
  , scoreGame
  )
  where

import Data.Foldable (foldlM)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Attoparsec.Text
import Data.Text qualified as T
import Debug.Pretty.Simple

traceLab :: Show a => String -> a -> a
traceLab lab x = pTrace (lab ++ ": " ++ show x) x

part1 :: String -> String
part1 input =
  let (calledNums, boards) = parseGame input
      score = scoreGame calledNums boards
  in show score

parseGame :: String -> ([Int], [Board])
parseGame input =
  case parseOnly bingoP $ T.pack input of
    Left err -> error $ "parse error: " ++ err
    Right res -> res

scoreGame :: [Int] -> [Board] -> Int
scoreGame calledNums boards =
  case applyCalledNumbersToBoards calledNums boards of
    Left s -> s
    Right _ -> error "no winning board"

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

applyCalledNumberToBoards :: Int -> [Board] -> Either Int [Board]
applyCalledNumberToBoards calledNum = traverse (applyCalledNumber calledNum)

applyCalledNumbersToBoards :: [Int] -> [Board] -> Either Int [Board]
applyCalledNumbersToBoards calledNums boards = foldlM (flip applyCalledNumberToBoards) boards calledNums
