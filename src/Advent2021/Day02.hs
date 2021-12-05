{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Advent2021.Day02
  ( part1
  , part2
  )
where

import Control.Monad ( (>=>) )
import Data.List ( foldl', stripPrefix )
import Safe ( readMay )

part1 :: String -> String
part1 input =
  let Position{..} = foldl' move (Position 0 0 0) $ parseCommand <$> lines input
  in show (horizontal * depth)

part2 :: String -> String
part2 input =
  let Position{..} = foldl' moveWithAim (Position 0 0 0) $ parseCommand <$> lines input
  in show (horizontal * depth)

data Command = Forward Int | Down Int | Up Int
  deriving (Show, Eq, Ord)

parseCommand :: String -> Command
parseCommand text = case text of
  (stripPrefix "forward " >=> readMay -> Just n) -> Forward n
  (stripPrefix "down "    >=> readMay -> Just n) -> Down n
  (stripPrefix "up "      >=> readMay -> Just n) -> Up n
  _ -> error $ "unable to parse command: " ++ text

data Position = Position { horizontal :: Int, depth :: Int, aim :: Int }
  deriving (Show, Eq, Ord)

move :: Position -> Command -> Position
move pos (Forward x) = pos { horizontal = horizontal pos + x }
move pos (Down x)    = pos { depth = depth pos + x }
move pos (Up x)      = pos { depth = depth pos - x }

moveWithAim :: Position -> Command -> Position
moveWithAim pos (Forward x) = pos { horizontal = horizontal pos + x, depth = depth pos + aim pos * x }
moveWithAim pos (Down x)    = pos { aim = aim pos + x }
moveWithAim pos (Up x)      = pos { aim = aim pos - x }
