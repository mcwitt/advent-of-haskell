{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module AOC.Y2021.D09 where

import AOC.Solution (Day (Day), Solution (Solution, day, year), Year (Year))
import qualified AOC.Solution as S
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Char as Char
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import Data.Maybe (mapMaybe)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Linear.V2 (V2 (V2))

data Heightmap = HM (Vector Int) Int

type Point = V2 Int

type Idx = Int

parseInput :: ByteString -> Heightmap
parseInput =
  heightmap . (fmap . fmap) Char.digitToInt
    . lines
    . Char8.unpack

heightmap :: [[Int]] -> Heightmap
heightmap rows = HM (V.fromList [z | row <- rows, z <- row]) $ length (head rows)

toList :: Heightmap -> [(Idx, Int)]
toList (HM zs _) = [0 ..] `zip` V.toList zs

i2p :: Int -> Idx -> V2 Int
i2p w = V2 <$> (`div` w) <*> (`mod` w)

p2i :: Int -> V2 Int -> Idx
p2i w (V2 i j) = i * w + j

lowPoints :: Heightmap -> [(Idx, Int)]
lowPoints hm =
  [ (x, z)
    | (x, z) <- toList hm,
      let ps = neighbors hm x,
      all ((> z) . snd) ps
  ]

neighbors :: Heightmap -> Idx -> [(Idx, Int)]
neighbors (HM zs w) = mapMaybe get . adjacent . i2p w
  where
    adjacent (V2 x y) = [V2 (x + 1) y, V2 x (y + 1), V2 (x -1) y, V2 x (y -1)]
    get p@(V2 _ j) | 0 <= j && j < w = let x = p2i w p in (x,) <$> zs V.!? x
    get _ = Nothing

solve1 :: Heightmap -> Int
solve1 hm = sum $ (+ 1) . snd <$> lowPoints hm

solve2 :: Heightmap -> Int
solve2 hm =
  product . take 3 . reverse . List.sort $
    [IntSet.size b | (x, _) <- lowPoints hm, let b = basin hm x]

basin :: Heightmap -> Idx -> IntSet
basin hm x = go [x] IntSet.empty
  where
    go [] b = b
    go (x : xs) b
      | x `IntSet.member` b = go xs b
      | otherwise = go (mapMaybe basinPoint (neighbors hm x) ++ xs) (IntSet.insert x b)
    basinPoint (x, z) = if z < 9 then Just x else Nothing

solution =
  Solution
    { year = Year 2021,
      day = Day 9,
      S.parseInput = parseInput,
      S.solve1 = solve1,
      S.solve2 = solve2
    }
