{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module AOC.Y2021.D06 where

import AOC.Solution (Day (Day), Solution (Solution, day, year), Year (Year))
import qualified AOC.Solution as S
import qualified Data.Attoparsec.ByteString.Char8 as P
import Data.ByteString (ByteString)
import Data.Foldable (fold)
import Data.IntMap.Monoidal (MonoidalIntMap)
import qualified Data.IntMap.Monoidal as IntMap
import Data.Monoid (Sum (..))

parseInput :: ByteString -> [Int]
parseInput s = let Right r = P.parseOnly input s in r
  where
    input = P.decimal `P.sepBy` ","

solve1 :: [Int] -> Int
solve1 = solve 80

solve :: Int -> [Int] -> Int
solve gen =
  getSum
    . fold
    . (!! gen)
    . iterate step
    . foldMap (`IntMap.singleton` Sum 1)

step :: MonoidalIntMap (Sum Int) -> MonoidalIntMap (Sum Int)
step = foldMap go . IntMap.toList
  where
    go = \case
      (0, n) -> IntMap.fromList [(8, n), (6, n)]
      (t, n) -> IntMap.singleton (t - 1) n

solve2 :: [Int] -> Int
solve2 = solve 256

solution :: Solution [Int] Int Int
solution =
  Solution
    { year = Year 2021,
      day = Day 6,
      S.parseInput = parseInput,
      S.solve1 = solve1,
      S.solve2 = solve2
    }
