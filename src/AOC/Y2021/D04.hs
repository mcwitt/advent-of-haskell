{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module AOC.Y2021.D04 where

import AOC.Solution (Day (Day), Solution (Solution, day, year), Year (Year))
import qualified AOC.Solution as S
import Control.Applicative (many)
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Data.List

type Board = [[Int]]

parseInput :: ByteString -> ([Int], [Board])
parseInput s = let Right r = parseOnly input s in r
  where
    input = (,) <$> (draws <* endOfLine <* endOfLine) <*> boards
    boards = board `sepBy1` endOfLine
    draws = decimal `sepBy1` ","
    board = count 5 (boardRow <* endOfLine)
    boardRow = many " " *> count 5 (decimal <* many " ")

solve1 :: ([Int], [Board]) -> Int
solve1 (draws, boards) =
  let (ds, board) =
        head
          [ (ds, board)
            | ds <- inits draws,
              board <- boards,
              complete ds board
          ]
   in score ds board

complete :: [Int] -> Board -> Bool
complete draws board = completeRows board || completeRows (transpose board)
  where
    completeRows = any $ all (`elem` draws)

score :: [Int] -> Board -> Int
score draws board =
  let sumUnmarked = sum $ fmap (sum . filter (not . (`elem` draws))) board
      lastCalled = last draws
   in sumUnmarked * lastCalled

solve2 :: ([Int], [Board]) -> Int
solve2 (draws, boards) =
  let (ds, board) =
        last
          [ (ds, board)
            | ds <- inits draws,
              board <- boards,
              complete ds board,
              not $ complete (init ds) board
          ]
   in score ds board

solution :: Solution ([Int], [Board]) Int Int
solution =
  Solution
    { year = Year 2021,
      day = Day 4,
      S.parseInput = parseInput,
      S.solve1 = solve1,
      S.solve2 = solve2
    }
