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
import Data.Foldable (foldlM)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Attoparsec.Text
import Data.Text qualified as T
import Safe

part1 :: String -> String
part1 input =
  let (calledNums, boards) = parseGame input
      score = getFirstWinningScore calledNums boards
  in show score

parseGame :: String -> ([Int], [Board])
parseGame input =
  case parseOnly bingoP $ T.pack input of
    Left err -> error $ "parse error: " ++ err
    Right res -> res

getFirstWinningScore :: [Int] -> [Board] -> Int
getFirstWinningScore calledNums boards =
  case applyCalledNumbersToBoards calledNums boards of
    Left s -> s
    Right _ -> error "no winning board"

part2 :: String -> String 
part2 input =
  let (calledNums, boards) = parseGame input
      score = getLastWinningScore calledNums boards
  in show score

getLastWinningScore :: [Int] -> [Board] -> Int
getLastWinningScore calledNums boards =
  case lastMay $ applyCalledNumbersToBoards' calledNums boards of
    Nothing -> error "no winning board"
    Just score -> score

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
applyCalledNumberToBoards = traverse . applyCalledNumber

applyCalledNumbersToBoards :: [Int] -> [Board] -> Either Int [Board]
applyCalledNumbersToBoards calledNums boards = foldlM (flip applyCalledNumberToBoards) boards calledNums

{-
Method for first part involved using Either monad, so that as soon as a winner was found, the computation aborted.
Now we need to find the last winner. Can we generate a list lazily so that the caller can drive the computation by
either taking the first winner or the last one?

This only works if you are producing a list, and can produce a cons as a result, with a thunk for the tail.
If you have to return a list and something else, you can't write it as a cons. But why can't we just produce
a list? The callers don't really need the boards, right?

applyCalledNumber for the first part returned Either Int Board. If there is a winner, it returns the score, but
if not, it returns the Board. For the second part, it can still do the same, because if the board has won, we don't
want to include it in the list anymore.

One issue is that multiple boards could potentially win for a given called number. Then it's not clear in which
order the winners should be ranked. Supposedly, the test data don't produce an ambiguous situation. But we have
to handle it somehow in the code. It seems easiest to assume that if there are several winners for a called number,
they are announced in board order.
-}

applyCalledNumberToBoards' :: Int -> [Board] -> ([Int], [Board])
applyCalledNumberToBoards' calledNum boards =
  let boardsAfterNumber = applyCalledNumber calledNum <$> boards
  in partitionEithers boardsAfterNumber

applyCalledNumbersToBoards' :: [Int] -> [Board] -> [Int]
applyCalledNumbersToBoards' _calledNums [] = [] -- don't need to call any more numbers because everyone has won
applyCalledNumbersToBoards' [] _boards = [] -- no more numbers to call, so there can be no more winners
applyCalledNumbersToBoards' (calledNum : calledNums') boards =
  let (winners, boards') = applyCalledNumberToBoards' calledNum boards
  in winners ++ applyCalledNumbersToBoards' calledNums' boards'
