{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AOC.Y2015.D07 where

import AOC.Solution (Day (Day), Solution (Solution, day, year), Year (Year))
import qualified AOC.Solution as S
import Control.Applicative ((<|>))
import Control.Arrow ((>>>))
import Data.Attoparsec.ByteString.Char8
  ( digit,
    endOfLine,
    letter_ascii,
    many1,
    parseOnly,
    sepBy,
  )
import Data.Bits
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Debug.Trace

type Signal = Int

type Wire = String

data Connection = Input :-> Wire deriving (Show)

data Value = Signal Signal | Wire Wire deriving (Show)

data Gate2 = And | Or | LShift | RShift deriving (Show)

data Gate1 = Not deriving (Show)

data Input
  = Raw Value
  | Op2 Gate2 Value Value
  | Op1 Gate1 Value
  deriving (Show)

parseInput :: ByteString -> [Connection]
parseInput s = let Right r = parseOnly inp s in r
  where
    inp = connection `sepBy` endOfLine
    connection = (:->) <$> input <* " -> " <*> many1 letter_ascii
    input = op2 <|> op1 <|> raw
    raw = Raw <$> value
    op2 = do
      x <- value
      _ <- " "
      g <- gate2
      _ <- " "
      y <- value
      pure $ Op2 g x y
    gate2 =
      And <$ "AND"
        <|> Or <$ "OR"
        <|> LShift <$ "LSHIFT"
        <|> RShift <$ "RSHIFT"
    op1 = Op1 <$> gate1 <* " " <*> value
    gate1 = Not <$ "NOT"
    value = signal <|> wire
    signal = Signal . read <$> many1 digit
    wire = Wire <$> many1 letter_ascii

run :: [Connection] -> Map Wire Signal
run = foldl step Map.empty

step :: Map Wire Signal -> Connection -> Map Wire Signal
step map (input :-> wire) =
  let signal = case input of
        Raw v -> eval v
        Op2 And v1 v2 -> eval v1 .&. eval v2
        Op2 Or v1 v2 -> eval v1 .|. eval v2
        Op2 LShift v s -> eval v `shift` eval s
        Op2 RShift v s -> eval v `shift` (- eval s)
        Op1 Not v -> complement $ eval v
   in Map.insert wire signal map
  where
    eval :: Value -> Signal
    eval (Signal s) = s
    eval (Wire w) = case Map.lookup w map of
      Just x -> x
      Nothing -> trace ("failed to lookup " <> w) 0

solve1 :: [Connection] -> Int
solve1 = run >>> (Map.! "a")

solution :: Solution [Connection] Int o2
solution =
  Solution
    { year = Year 2015,
      day = Day 7,
      S.parseInput = parseInput,
      S.solve1 = solve1,
      S.solve2 = undefined
    }
