{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module AOC.Y2021.D11 where

import AOC.Solution
  ( Day (Day),
    Solution (Solution, day, year),
    Year (Year),
  )
import qualified AOC.Solution as S
import Control.Monad.Writer (runWriter, writer)
import Data.Bifunctor (Bifunctor (second))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Char (digitToInt, intToDigit)
import Data.List (find, foldl', scanl')
import Data.Maybe (maybeToList)
import Data.Monoid (Sum (getSum))
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Linear (V2 (V2))

data Octopus = Charging Int | Discharged deriving (Eq, Show)

data Grid a = Grid Int (Vector a) deriving (Eq)

instance Functor Grid where
  fmap f (Grid w v) = Grid w $ V.map f v

instance Foldable Grid where
  foldMap f (Grid _ v) = V.foldMap f v

instance Traversable Grid where
  traverse f (Grid w v) = Grid w <$> traverse f v

type Ix = Int

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = go
  where
    go [] = []
    go xs = let (ys, zs) = splitAt n xs in ys : go zs

instance Show (Grid Octopus) where
  show (Grid w v) = unlines [[f octo | octo <- row] | row <- chunksOf w (V.toList v)]
    where
      f (Charging e) = intToDigit e
      f Discharged = 'X'

parseInput :: ByteString -> Grid Octopus
parseInput bs =
  let rows@(firstRow : _) = lines $ unpack bs
      xs = [Charging n | row <- rows, c <- row, let n = digitToInt c]
   in Grid (length firstRow) (V.fromList xs)

type Point = V2 Int

point :: Grid a -> Int -> Int -> Maybe (V2 Int)
point (Grid w _) i j
  | 0 <= i && i < w && 0 <= j && j < w = Just (V2 i j)
  | otherwise = Nothing

i2p :: Grid a -> Ix -> V2 Int
i2p (Grid w _) = V2 <$> (`div` w) <*> (`mod` w)

p2i :: Grid a -> V2 Int -> Ix
p2i (Grid w _) (V2 i j) = i * w + j

neighbors :: Grid Octopus -> Ix -> [Ix]
neighbors g ix =
  let V2 i j = i2p g ix
   in [ p2i g p
        | di <- [-1, 0, 1],
          dj <- [-1, 0, 1],
          di /= 0 || dj /= 0,
          p <- maybeToList $ point g (i + di) (j + dj)
      ]

flash :: Grid Octopus -> Ix -> Grid Octopus
flash g@(Grid w v) ix =
  let update mv = go
        where
          go ix = MV.read mv ix >>= visit ix

          visit ix = \case
            Charging n | n < 9 -> MV.write mv ix $ Charging (succ n)
            Charging n | n >= 9 -> do
              MV.write mv ix Discharged
              mapM_ go (neighbors g ix)
            _ -> pure ()

      v' = V.modify (`update` ix) v
   in Grid w v'

sweep :: (Grid a -> Ix -> Grid a) -> Grid a -> Grid a
sweep f g@(Grid _ v) = foldl' f g [0 .. V.length v - 1]

reset :: Octopus -> (Octopus, Sum Int)
reset = \case
  Charging n -> (Charging n, 0)
  Discharged -> (Charging 0, 1)

update :: Grid Octopus -> (Grid Octopus, Sum Int)
update = runWriter . traverse (writer . reset) . sweep flash

flashes :: Grid Octopus -> [Int]
flashes g = snd <$> iterate (\(g, _) -> second getSum $ update g) (g, 0)

solution :: Solution (Grid Octopus) Int Int
solution =
  Solution
    { year = Year 2021,
      day = Day 11,
      S.parseInput = parseInput,
      S.solve1 = (!! 100) . scanl' (+) 0 . flashes,
      S.solve2 = \g@(Grid w _) ->
        let Just (_, step) =
              find (\(n, _) -> n == w * w) $
                zip (flashes g) [0 :: Int ..]
         in step
    }
