{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module AOC.Y2021.D12 where

import AOC.Solution
  ( Day (Day),
    Solution (Solution, day, year),
    Year (Year),
  )
import qualified AOC.Solution as S
import qualified Data.Attoparsec.ByteString.Char8 as P
import Data.ByteString (ByteString)
import Data.Char (isLower, isUpper, toLower)
import Text.RawString.QQ

type Input = [Passage]

type Passage = (Cave, Cave)

data Cave = Start | End | Cave Size Label deriving (Eq, Show)

data Size = Small | Big deriving (Eq, Show)

type Label = String

parseInput :: ByteString -> Input
parseInput s = let Right r = P.parseOnly input s in r
  where
    input = passage `P.sepBy1` P.endOfLine
    passage = (,) <$> cave <* "-" <*> cave
    cave =
      P.choice
        [ Start <$ "start",
          End <$ "end",
          Cave Small <$> P.many1 (P.satisfy isLower),
          Cave Big . fmap toLower <$> P.many1 (P.satisfy isUpper)
        ]

ex :: Input
ex =
  parseInput
    [r|start-A
start-b
A-c
A-b
b-d
A-end
b-end|]

solution =
  Solution
    { year = Year 2021,
      day = Day 11,
      S.parseInput = parseInput,
      S.solve1 = undefined,
      S.solve2 = undefined
    }
