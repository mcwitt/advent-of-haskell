{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module AOC.Y2021.D13 where

import AOC.Solution
  ( Day (Day),
    Solution (Solution, day, year),
    Year (Year),
  )
import qualified AOC.Solution as S
import Control.Applicative (Alternative ((<|>)))
import qualified Data.Attoparsec.ByteString.Char8 as P
import Data.ByteString (ByteString)
import Data.List (foldl', intercalate, nub)
import Linear (V2 (V2))

data Line = V Int | H Int deriving (Show)

type Dot = V2 Int

newtype Dots = Dots {unDots :: [Dot]}

instance Show Dots where
  show (Dots xs) = intercalate "\n" lines
    where
      lines = fmap mkLine [0 .. ymax]
      mkLine y = [if V2 x y `elem` xs then '#' else '.' | x <- [0 .. xmax]]
      xmax = maximum [x | V2 x _ <- xs]
      ymax = maximum [y | V2 _ y <- xs]

dots :: [V2 Int] -> Dots
dots = Dots . nub

data Input = Input Dots [Line] deriving (Show)

parseInput :: ByteString -> Input
parseInput s = let Right r = P.parseOnly input s in r
  where
    input =
      Input . dots
        <$> dot `P.sepBy1` P.endOfLine
        <* P.endOfLine
        <* P.endOfLine
        <*> line `P.sepBy1` P.endOfLine
    dot = V2 <$> nat <* "," <*> nat
    line = "fold along " *> (V <$ "x=" <*> nat <|> H <$ "y=" <*> nat)
    nat = read <$> P.many1 P.digit

foldAlong :: Dots -> Line -> Dots
foldAlong (Dots ds) = \case
  V x0 -> dots [V2 (reflect x0 x) y | V2 x y <- ds]
  H y0 -> dots [V2 x (reflect y0 y) | V2 x y <- ds]
  where
    reflect x0 x = if x < x0 then x else 2 * x0 - x

solution :: Solution Input Int Dots
solution =
  Solution
    { year = Year 2021,
      day = Day 13,
      S.parseInput = parseInput,
      S.solve1 = \(Input ds (l : _)) -> length . unDots $ ds `foldAlong` l,
      S.solve2 = \(Input ds ls) -> foldl' foldAlong ds ls
    }
