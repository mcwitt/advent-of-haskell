{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module AOC.Y2021.D08 where

import AOC.Solution (Day (Day), Solution (Solution, day, year), Year (Year))
import qualified AOC.Solution as S
import qualified Data.Attoparsec.ByteString.Char8 as P
import Data.ByteString (ByteString)
import Data.Functor (($>))
import qualified Data.List as List
import qualified Data.Maybe as Maybe

data SignalLine = A | B | C | D | E | F | G deriving (Eq, Ord, Show)

type Pattern = [SignalLine]

type Sequence = [Pattern]

parseInput :: ByteString -> [(Sequence, Sequence)]
parseInput s = let Right r = P.parseOnly input s in r
  where
    input = inputLine `P.sepBy` P.endOfLine
    inputLine = (,) <$> P.many1 signalLine `P.sepBy` " " <* " | " <*> (P.many1 signalLine `P.sepBy` " ")
    signalLine =
      P.choice
        [ A <$ "a",
          B <$ "b",
          C <$ "c",
          D <$ "d",
          E <$ "e",
          F <$ "f",
          G <$ "g"
        ]

solve1 :: [(Sequence, Sequence)] -> Int
solve1 ss =
  sum
    [ if length p == 2
        || length p == 4
        || length p == 3
        || length p == 7
        then 1
        else 0
      | (_, ps) <- ss,
        p <- ps
    ]

solve2 :: [(Sequence, Sequence)] -> Int
solve2 = sum . fmap (uncurry solveDisplay)

solveDisplay :: Sequence -> Sequence -> Int
solveDisplay ps out =
  let signals = [A, B, C, D, E, F, G]
      bijections = zip signals <$> List.permutations signals
      f = head $ Maybe.mapMaybe (\f -> mapDecode f ps $> f) bijections
      mapDecode f ps = decode $ fmap (map' f) ps
      Just result = mapDecode f out
   in result

map' :: Eq a => [(a, b)] -> [a] -> [b]
map' ps = fmap $ Maybe.fromJust . (`lookup` ps)

decode :: Sequence -> Maybe Int
decode ps = fromDecimal <$> traverse patternToDigit ps
  where
    fromDecimal ds = sum [d * 10 ^ p | (d, p) <- zip (reverse ds) [0 :: Int ..]]

patternToDigit :: Pattern -> Maybe Int
patternToDigit xs = case List.sort xs of
  [A, B, C, E, F, G] -> Just 0
  [C, F] -> Just 1
  [A, C, D, E, G] -> Just 2
  [A, C, D, F, G] -> Just 3
  [B, C, D, F] -> Just 4
  [A, B, D, F, G] -> Just 5
  [A, B, D, E, F, G] -> Just 6
  [A, C, F] -> Just 7
  [A, B, C, D, E, F, G] -> Just 8
  [A, B, C, D, F, G] -> Just 9
  _ -> Nothing

solution =
  Solution
    { year = Year 2021,
      day = Day 8,
      S.parseInput = parseInput,
      S.solve1 = solve1,
      S.solve2 = solve2
    }
