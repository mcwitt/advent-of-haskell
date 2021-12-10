{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module AOC.Y2021.D10 where

import AOC.Solution (Day (Day), Solution (Solution, day, year), Year (Year))
import qualified AOC.Solution as S
import Control.Monad (foldM)
import qualified Data.Attoparsec.ByteString.Char8 as P
import Data.ByteString (ByteString)
import qualified Data.List as List
import Data.Maybe (mapMaybe)

data DelimType = Paren | Bracket | Brace | Angle deriving (Eq, Show)

data DelimVariant = Opening | Closing deriving (Eq, Show)

data Delimiter = D DelimVariant DelimType deriving (Eq, Show)

parseInput :: ByteString -> [[Delimiter]]
parseInput s = let Right r = P.parseOnly input s in r
  where
    input = inputLine `P.sepBy` P.endOfLine
    inputLine =
      P.many1 $
        P.choice
          [ D Opening Paren <$ "(",
            D Opening Bracket <$ "[",
            D Opening Brace <$ "{",
            D Opening Angle <$ "<",
            D Closing Paren <$ ")",
            D Closing Bracket <$ "]",
            D Closing Brace <$ "}",
            D Closing Angle <$ ">"
          ]

solve1 :: [[Delimiter]] -> Int
solve1 = sum . fmap score
  where
    score xs = case checkLine xs of
      Left t -> scoreError t
      _ -> 0

checkLine :: [Delimiter] -> Either DelimType [DelimType]
checkLine = foldM go []
  where
    go [] (D Opening x) = Right [x]
    go [] (D Closing x) = Left x
    go xs (D Opening x) = Right $ x : xs
    go (x : xs) (D Closing x') = if x == x' then Right xs else Left x'

scoreError :: DelimType -> Int
scoreError = \case
  Paren -> 3
  Bracket -> 57
  Brace -> 1197
  Angle -> 25137

solve2 :: [[Delimiter]] -> Int
solve2 xss =
  let hush = \case
        Right x -> Just x
        Left _ -> Nothing
      scores = mapMaybe (fmap scoreCompletion . hush . checkLine) xss
   in List.sort scores !! (length scores `div` 2)

scoreCompletion :: [DelimType] -> Int
scoreCompletion = List.foldl' (\s t -> s * 5 + score t) 0
  where
    score = \case
      Paren -> 1
      Bracket -> 2
      Brace -> 3
      Angle -> 4

solution =
  Solution
    { year = Year 2021,
      day = Day 10,
      S.parseInput = parseInput,
      S.solve1 = solve1,
      S.solve2 = solve2
    }
