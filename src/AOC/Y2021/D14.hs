{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module AOC.Y2021.D14 where

import AOC.Solution
  ( Day (Day),
    Solution (Solution, day, year),
    Year (Year),
  )
import qualified AOC.Solution as S
import qualified Data.Attoparsec.ByteString.Char8 as P
import Data.ByteString (ByteString)
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map

type Input = (Polymer, [Rule])

type Polymer = [Mer]

type Mer = Char

data Rule = Pair Mer :-> Mer

data Pair a = P a a deriving (Eq, Ord)

parseInput :: ByteString -> Input
parseInput s = let Right r = P.parseOnly input s in r
  where
    input = (,) <$> template <* P.endOfLine <* P.endOfLine <*> rule `P.sepBy1` P.endOfLine
    template = P.many1 mer
    mer = P.letter_ascii
    rule = (:->) <$> pair mer <* " -> " <*> mer
    pair p = P <$> p <*> p

match :: Pair Mer -> [Rule] -> Maybe Mer
match (P x y) = fmap (\(_ :-> z) -> z) . find (\(P x' y' :-> _) -> x == x' && y == y')

pairCounts :: Polymer -> Map (Pair Mer) Integer
pairCounts xs = Map.fromListWith (+) [(P x y, 1) | (x, y) <- zip xs (tail xs)]

step :: [Rule] -> Map (Pair Mer) Integer -> Map (Pair Mer) Integer
step rs mp =
  Map.fromListWith
    (+)
    [ (p, n)
      | (P x z, n) <- Map.toList mp,
        p <- case match (P x z) rs of
          Just y -> [P x y, P y z]
          Nothing -> [P x z]
    ]

solve :: Int -> Input -> Integer
solve n (xs, rs) =
  let pairCounts' = iterate (step rs) (pairCounts xs) !! n
      counts = (last xs, 1) : [(x, n) | (P x _, n) <- Map.toList pairCounts']
      ns = Map.elems $ Map.fromListWith (+) counts
   in maximum ns - minimum ns

solution :: Solution Input Integer Integer
solution =
  Solution
    { year = Year 2021,
      day = Day 14,
      S.parseInput = parseInput,
      S.solve1 = solve 10,
      S.solve2 = solve 40
    }
