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
solve1 = length . (!! 80) . iterate step

step :: [Int] -> [Int]
step ts =
  let (countNew, ts') = foldl go (0, []) ts
   in ts' ++ replicate countNew 8
  where
    go (c, ts') t = if t == 0 then (c + 1, 6 : ts') else (c, t - 1 : ts')

solve2 :: [Int] -> Int
solve2 =
  getSum
    . fold
    . (!! 256)
    . iterate step2
    . foldMap (`IntMap.singleton` Sum 1)

step2 :: MonoidalIntMap (Sum Int) -> MonoidalIntMap (Sum Int)
step2 = foldMap go . IntMap.toList
  where
    go = \case
      (0, Sum n) -> IntMap.fromList [(8, Sum n), (6, Sum n)]
      (t, Sum n) -> IntMap.singleton (t - 1) (Sum n)

solution =
  Solution
    { year = Year 2021,
      day = Day 6,
      S.parseInput = parseInput,
      S.solve1 = solve1,
      S.solve2 = solve2
    }
