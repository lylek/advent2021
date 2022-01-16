{-# LANGUAGE StrictData #-}

module Advent2021.Day10
  ( part1
  , part2
  ) where

part1 :: String -> String
part1 = show . sum . map (valueSyntaxError . parseDelimiters . tokenizeLine) . lines

part2 :: String -> String
part2 = id

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

data ParseResult = Incomplete | Corrupted Delimiter | Valid
  deriving (Show, Eq, Ord)

parseDelimiters :: [Either Delimiter Delimiter] -> ParseResult
parseDelimiters = go []
  where
    go [] [] = Valid
    go _  [] = Incomplete
    go ss (Left d : ds) = go (d:ss) ds
    go [] (Right d : _) = Corrupted d
    go (s:ss) (Right d : ds) | s == d    = go ss ds
                             | otherwise = Corrupted d

valueSyntaxError :: ParseResult -> Int
valueSyntaxError Incomplete = 0
valueSyntaxError (Corrupted Parenthesis) = 3
valueSyntaxError (Corrupted SquareBracket) = 57
valueSyntaxError (Corrupted CurlyBrace) = 1197
valueSyntaxError (Corrupted AngleBracket) = 25137
valueSyntaxError Valid = 0
