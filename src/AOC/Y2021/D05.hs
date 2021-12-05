{-# LANGUAGE OverloadedStrings #-}

module AOC.Y2021.D05 where

import AOC.Solution (Day (Day), Solution (Solution, day, year), Year (Year))
import qualified AOC.Solution as S
import Data.Attoparsec.ByteString.Char8 (decimal, endOfLine, parseOnly, sepBy)
import Data.ByteString (ByteString)
import qualified Data.Map as Map
import Linear.V2 (V2 (..))

type Point = V2 Int

data LineSegment = LS Point Point deriving (Show)

parseInput :: ByteString -> [LineSegment]
parseInput s = let Right r = parseOnly input s in r
  where
    input = line `sepBy` endOfLine
    line = LS <$> v2 <* " -> " <*> v2
    v2 = V2 <$> decimal <* "," <*> decimal

solve1 :: [LineSegment] -> Int
solve1 ls =
  let ps = filter notDiagonal ls >>= points
      diagram = Map.fromListWith (+) $ zip ps $ repeat 1
   in count (>= 2) $ Map.elems diagram
  where
    notDiagonal (LS (V2 x1 y1) (V2 x2 y2)) = x1 == x2 || y1 == y2

points :: LineSegment -> [Point]
points (LS (V2 x1 y1) (V2 x2 y2)) =
  [V2 x y | (x, y) <- zipLongest x1 y1 (points1 x1 x2) (points1 y1 y2)]
  where
    points1 i f = if f - i > 0 then [i .. f] else reverse [f .. i]

zipLongest :: a -> b -> [a] -> [b] -> [(a, b)]
zipLongest defX defY = go
  where
    go [] [] = []
    go (x : xs) [] = (x, defY) : go xs []
    go [] (y : ys) = (defX, y) : go [] ys
    go (x : xs) (y : ys) = (x, y) : go xs ys

count :: Foldable f => (a -> Bool) -> f a -> Int
count p = foldr (\x z -> if p x then z + 1 else z) 0

solve2 :: [LineSegment] -> Int
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
