{-# LANGUAGE LambdaCase #-}

module AOC.Y2015.D05 where

import AOC.Solution (Day (Day), Solution (Solution, day, year), Year (Year))
import qualified AOC.Solution as S
import Control.Arrow ((>>>))
import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.ByteString.Char8 as Char8
import Data.Functor.Contravariant (Predicate (..))
import Data.List (isInfixOf)

nice :: Predicate String
nice =
  foldMap
    Predicate
    [ count vowel >>> (>= 3),
      stutter,
      \s -> not $ any (`isInfixOf` s) ["ab", "cd", "pq", "xy"]
    ]
  where
    vowel = flip elem ['a', 'e', 'i', 'o', 'u']

count :: (a -> Bool) -> [a] -> Int
count p = foldr go 0
  where
    go x z = if p x then succ z else z

stutter :: String -> Bool
stutter = \case
  x : xs@(y : _) -> x == y || stutter xs
  _ -> False

nice2 :: Predicate String
nice2 = foldMap Predicate [pair, sandwich]

uninterleave :: [a] -> ([a], [a])
uninterleave = foldr (\x (ys, zs) -> (zs, x : ys)) ([], [])

pair :: String -> Bool
pair = \case
  x : y : xs | [x, y] `isInfixOf` xs -> True
  _ : xs -> pair xs
  _ -> False

sandwich :: String -> Bool
sandwich = uninterleave >>> bimap stutter stutter >>> uncurry (||)

solution :: Solution [String] Int Int
solution =
  Solution
    { year = Year 2015,
      day = Day 5,
      S.parseInput = fmap Char8.unpack . Char8.lines,
      S.solve1 = count $ getPredicate nice,
      S.solve2 = count $ getPredicate nice2
    }
