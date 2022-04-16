{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module AOC.Y2021.D15 where

import AOC.Solution
  ( Day (Day),
    Solution (Solution, day, year),
    Year (Year),
  )
import qualified AOC.Solution as S
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char (digitToInt, intToDigit)
import Data.List (intercalate)
import Data.Maybe (maybeToList)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Linear.V2
import Text.RawString.QQ (r)

data Grid a = Grid Int (Vector a)

type Ix = Int

instance Show (Grid Int) where
  show (Grid width xs) = intercalate "\n" [fmap intToDigit cs | cs <- chunksOf width (V.toList xs)]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (ys, zs) = splitAt n xs in ys : chunksOf n zs

parseInput :: ByteString -> Grid Int
parseInput s =
  let xss@(xs : _) = (fmap . fmap) digitToInt . lines $ BS.unpack s
   in Grid (length xs) . V.fromList $ concat xss

type Point = V2 Int

point :: Grid a -> Int -> Int -> Maybe (V2 Int)
point (Grid w _) i j
  | 0 <= i && i < w && 0 <= j && j < w = Just (V2 i j)
  | otherwise = Nothing

i2p :: Grid a -> Ix -> V2 Int
i2p (Grid w _) = V2 <$> (`div` w) <*> (`mod` w)

p2i :: Grid a -> V2 Int -> Ix
p2i (Grid w _) (V2 i j) = i * w + j

neighbors :: Grid a -> Ix -> [Ix]
neighbors g ix =
  let V2 i j = i2p g ix
   in [ p2i g p
        | di <- [-1, 0, 1],
          dj <- [-1, 0, 1],
          di /= 0 || dj /= 0,
          p <- maybeToList $ point g (i + di) (j + dj)
      ]

ex :: ByteString
ex =
  [r|1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581|]

solution =
  Solution
    { year = Year 2021,
      day = Day 15,
      S.parseInput = parseInput,
      S.solve1 = undefined,
      S.solve2 = undefined
    }
