{-# LANGUAGE OverloadedStrings #-}

module AOC.Y2021.D02 where

import AOC.Solution (Day (Day), Solution (Solution, day, year), Year (Year))
import qualified AOC.Solution as S
import Control.Applicative (Alternative ((<|>)))
import Data.Attoparsec.ByteString (parseOnly, sepBy)
import Data.Attoparsec.ByteString.Char8 (decimal, endOfLine, space)
import Data.ByteString (ByteString)

data Op = Forward | Down | Up

type Instr = (Op, Int)

parseInput :: ByteString -> [Instr]
parseInput s = let Right r = parseOnly input s in r
  where
    input = move `sepBy` endOfLine
    move = (,) <$> direction <* space <*> decimal
    direction = Forward <$ "forward" <|> Down <$ "down" <|> Up <$ "up"

solve1 :: [Instr] -> Int
solve1 ms = let (x, y) = foldl update (0, 0) ms in x * y
  where
    update (x, y) (op, v) = case op of
      Forward -> (x + v, y)
      Down -> (x, y + v)
      Up -> (x, max 0 (y - v))

solve2 :: [Instr] -> Int
solve2 ms = let (x, y, _) = foldl update (0, 0, 0) ms in x * y
  where
    update (x, y, aim) (op, v) = case op of
      Forward -> (x + v, y + v * aim, aim)
      Down -> (x, y, aim + v)
      Up -> (x, y, aim - v)

solution :: Solution [Instr] Int Int
solution =
  Solution
    { year = Year 2021,
      day = Day 2,
      S.parseInput = parseInput,
      S.solve1 = solve1,
      S.solve2 = solve2
    }
