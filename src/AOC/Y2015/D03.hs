{-# LANGUAGE LambdaCase #-}

module AOC.Y2015.D03 where

import AOC.Solution (Day (Day), Solution (Solution, day, year), Year (Year))
import qualified AOC.Solution as S
import Control.Applicative (many, (<|>))
import Data.Attoparsec.ByteString (parseOnly)
import Data.Attoparsec.ByteString.Char8 (char)
import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString.Char8 (ByteString)
import Data.Functor (($>))
import Data.List (scanl')
import Data.Monoid (Sum (..))
import qualified Data.Set as Set

type V2 = (Sum Int, Sum Int)

parseInput :: ByteString -> [V2]
parseInput s = let Right r = parseOnly input s in r
  where
    input = many heading
    heading =
      char '^' $> (0, 1)
        <|> char '>' $> (1, 0)
        <|> char 'v' $> (0, -1)
        <|> char '<' $> (-1, 0)

mconcats :: Monoid a => [a] -> [a]
mconcats = scanl' (<>) mempty

countDistinct :: Ord a => [a] -> Int
countDistinct = Set.size . Set.fromList

solve1 :: [V2] -> Int
solve1 = countDistinct . mconcats

uninterleave :: [a] -> ([a], [a])
uninterleave = foldr (\x (xs, ys) -> (x : ys, xs)) ([], [])

solve2 :: [V2] -> Int
solve2 =
  countDistinct
    . uncurry (<>)
    . bimap mconcats mconcats
    . uninterleave

solution :: Solution [V2] Int Int
solution =
  Solution
    { year = Year 2015,
      day = Day 3,
      S.parseInput = parseInput,
      S.solve1 = solve1,
      S.solve2 = solve2
    }
