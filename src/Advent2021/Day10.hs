{-# LANGUAGE StrictData #-}

module Advent2021.Day10
  ( part1
  , part2
  ) where

import Data.Maybe (mapMaybe)
import Data.List (sort)

part1 :: String -> String
part1 = show . sum . map (scoreSyntaxError . parseDelimiters . tokenizeLine) . lines

part2 :: String -> String
part2 = show . scoreIncompletes . map (parseDelimiters . tokenizeLine) . lines

data Delimiter = Parenthesis | SquareBracket | CurlyBrace | AngleBracket
  deriving (Show, Eq, Ord)

tokenizeLine :: String -> [Either Delimiter Delimiter]
tokenizeLine = map tokenizeChar

tokenizeChar :: Char -> Either Delimiter Delimiter
tokenizeChar '(' = Left Parenthesis
tokenizeChar ')' = Right Parenthesis
tokenizeChar '[' = Left SquareBracket
tokenizeChar ']' = Right SquareBracket
tokenizeChar '{' = Left CurlyBrace
tokenizeChar '}' = Right CurlyBrace
tokenizeChar '<' = Left AngleBracket
tokenizeChar '>' = Right AngleBracket
tokenizeChar c   = error $ "Unrecognized character: " ++ show c

data ParseResult = Incomplete [Delimiter] | Corrupted Delimiter | Valid
  deriving (Show, Eq, Ord)

parseDelimiters :: [Either Delimiter Delimiter] -> ParseResult
parseDelimiters = go []
  where
    go [] [] = Valid
    go es  [] = Incomplete es
    go es (Left d : ds) = go (d:es) ds
    go [] (Right d : _) = Corrupted d
    go (e:es) (Right d : ds) | e == d    = go es ds
                             | otherwise = Corrupted d

scoreSyntaxError :: ParseResult -> Int
scoreSyntaxError Incomplete{} = 0
scoreSyntaxError (Corrupted Parenthesis) = 3
scoreSyntaxError (Corrupted SquareBracket) = 57
scoreSyntaxError (Corrupted CurlyBrace) = 1197
scoreSyntaxError (Corrupted AngleBracket) = 25137
scoreSyntaxError Valid = 0

scoreIncompletes :: [ParseResult] -> Int
scoreIncompletes rs =
  let is = mapMaybe scoreIncomplete rs
  in sort is !! (length is `div` 2)

scoreIncomplete :: ParseResult -> Maybe Int
scoreIncomplete (Incomplete es) = Just $ scoreExpected es
scoreIncomplete _ = Nothing

scoreExpected :: [Delimiter] -> Int
scoreExpected = go 0
  where
    go v [] = v
    go v (d : ds) = go (v * 5 + valueOf d) ds
    valueOf Parenthesis   = 1
    valueOf SquareBracket = 2
    valueOf CurlyBrace    = 3
    valueOf AngleBracket  = 4
