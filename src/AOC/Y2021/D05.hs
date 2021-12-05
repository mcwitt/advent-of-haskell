{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module AOC.Y2021.D05 where

import AOC.Solution (Day (Day), Solution (Solution, day, year), Year (Year))
import qualified AOC.Solution as S
import Data.Attoparsec.ByteString.Char8 (decimal, endOfLine, parseOnly, sepBy)
import Data.ByteString (ByteString)
import qualified Data.Map as Map
import Linear.V2

type Point = V2 Int

data Line = L Point Point deriving (Show)

parseInput :: ByteString -> [Line]
parseInput s = let Right r = parseOnly input s in r
  where
    input = line `sepBy` endOfLine
    line = L <$> v2 <* " -> " <*> v2
    v2 = V2 <$> decimal <* "," <*> decimal

solve1 :: [Line] -> Int
solve1 ls =
  let ps = filter (\(L (V2 x1 y1) (V2 x2 y2)) -> x1 == x2 || y1 == y2) ls >>= points
      diagram = Map.fromListWith (+) $ zip ps $ repeat 1
   in count (>= 2) $ Map.elems diagram

points :: Line -> [Point]
points (L (V2 x1 y1) (V2 x2 y2))
  | x1 == x2 = [V2 x1 y | y <- range y1 y2]
  | y1 == y2 = [V2 x y1 | x <- range x1 x2]
  | otherwise = [V2 x y | (x, y) <- zip (range x1 x2) (range y1 y2)]
  where
    range x y =
      let ival = [min x y .. max x y]
       in if x < y then ival else reverse ival

count :: Foldable f => (a -> Bool) -> f a -> Int
count p = foldr (\x z -> if p x then z + 1 else z) 0

solve2 :: [Line] -> Int
solve2 ls =
  let ps = ls >>= points
      diagram = Map.fromListWith (+) $ zip ps $ repeat 1
   in count (>= 2) $ Map.elems diagram

solution =
  Solution
    { year = Year 2021,
      day = Day 5,
      S.parseInput = parseInput,
      S.solve1 = solve1,
      S.solve2 = solve2
    }
