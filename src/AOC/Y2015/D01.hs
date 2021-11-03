{-# LANGUAGE LambdaCase #-}

module AOC.Y2015.D01 where

import AOC.Solution (Day (Day), Solution (Solution, day, year), Year (Year))
import qualified AOC.Solution as S
import Data.ByteString.Char8 (unpack)

type Input = String

solve1 :: Input -> Int
solve1 = sum . fmap translate

solve2 :: Input -> Int
solve2 = (+ 1) . length . takeWhile (>= 0) . scanl (+) 0 . fmap translate

translate :: Char -> Int
translate = \case
  '(' -> 1
  ')' -> -1
  _ -> 0

solution :: Solution String Int Int
solution =
  Solution
    { year = Year 2015,
      day = Day 1,
      S.parseInput = unpack,
      S.solve1 = solve1,
      S.solve2 = solve2
    }
