{-# LANGUAGE OverloadedStrings #-}

module AOC.Y2021.D06 where

import AOC.Solution (Day (Day), Solution (Solution, day, year), Year (Year))
import qualified AOC.Solution as S
import qualified Data.Attoparsec.ByteString.Char8 as P
import Data.ByteString (ByteString)
import qualified Data.IntMap.Monoidal as IntMap
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Semigroup (mtimesDefault)

parseInput :: ByteString -> [Int]
parseInput s = let Right r = P.parseOnly input s in r
  where
    input = P.decimal `P.sepBy` ","

solve1 :: [Int] -> Int
solve1 = solve 80

solve :: Int -> [Int] -> Int
solve gen ts =
  let initCounts = IntMap.fromListWith (+) $ zip ts $ repeat (1 :: Int)
      initState = [fromMaybe 0 $ IntMap.lookup i initCounts | i <- [0 .. 8]]
      propagator = mtimesDefault gen transition
   in sum $ matrixVectorProduct propagator initState

newtype IntMatrix = M [[Int]] deriving (Show)

instance Semigroup IntMatrix where
  M x <> M y = M [[sum $ zipWith (*) r c | c <- List.transpose y] | r <- x]

instance Monoid IntMatrix where
  mempty = undefined

type IntVector = [Int]

matrixVectorProduct :: IntMatrix -> IntVector -> IntVector
matrixVectorProduct (M rs) v = fmap (sum . zipWith (*) v) rs

transition :: IntMatrix
transition =
  M
    [ [0, 1, 0, 0, 0, 0, 0, 0, 0],
      [0, 0, 1, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 1, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 1, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 1, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 1, 0, 0],
      [1, 0, 0, 0, 0, 0, 0, 1, 0],
      [0, 0, 0, 0, 0, 0, 0, 0, 1],
      [1, 0, 0, 0, 0, 0, 0, 0, 0]
    ]

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
