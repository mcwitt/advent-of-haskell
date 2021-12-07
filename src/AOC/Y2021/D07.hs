{-# LANGUAGE OverloadedStrings #-}

module AOC.Y2021.D07 where

import AOC.Solution (Day (Day), Solution (Solution, day, year), Year (Year))
import qualified AOC.Solution as S
import qualified Data.Attoparsec.ByteString.Char8 as P
import Data.ByteString (ByteString)
import qualified Data.List as List

parseInput :: ByteString -> [Int]
parseInput s = let Right r = P.parseOnly input s in r
  where
    input = P.decimal `P.sepBy` ","

solve1 :: [Int] -> Int
solve1 xs = let median = List.sort xs !! (length xs `div` 2) in totalFuel id median xs

totalFuel :: (Int -> Int) -> Int -> [Int] -> Int
totalFuel fuel xopt xs = sum [fuel $ abs (x - xopt) | x <- xs]

solve2 :: [Int] -> Int
solve2 xs =
  let fuel d = d * (d + 1) `div` 2
      meanFloor = sum xs `div` length xs
   in totalFuel fuel meanFloor xs

solution :: Solution [Int] Int Int
solution =
  Solution
    { year = Year 2021,
      day = Day 7,
      S.parseInput = parseInput,
      S.solve1 = solve1,
      S.solve2 = solve2
    }
