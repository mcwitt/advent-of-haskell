{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module AOC.Y2015.D06 where

import AOC.Solution (Day (Day), Solution (Solution, day, year), Year (Year))
import qualified AOC.Solution as S
import Control.Applicative ((<|>))
import Control.Monad (forM_)
import Control.Monad.ST
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Data.List (foldl')
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as M

data Op
  = TurnOn
  | Toggle
  | TurnOff
  deriving (Show)

data Point = Point Int Int deriving (Show)

instance Enum Point where
  toEnum i = uncurry Point $ i `divMod` gridSize
  fromEnum (Point r c) = gridSize * r + c

gridSize :: Int
gridSize = 1000

data Rect = Rect Point Point deriving (Show)

type Instr = (Op, Rect)

parseInput :: ByteString -> [Instr]
parseInput s = let Right r = parseOnly input s in r
  where
    input = instruction `sepBy` endOfLine
    instruction =
      (TurnOn,) <$ "turn on " <*> rect
        <|> (Toggle,) <$ "toggle " <*> rect
        <|> (TurnOff,) <$ "turn off " <*> rect
    rect = Rect <$> index <* " through " <*> index
    index = Point <$> decimal <* "," <*> decimal

type Lights = Vector Bool

exec :: Instr -> Lights -> Lights
exec = \case
  (TurnOn, rect) -> modifyRect rect (\v i -> M.write v i True)
  (Toggle, rect) -> modifyRect rect (`M.modify` not)
  (TurnOff, rect) -> modifyRect rect (\v i -> M.write v i False)

modifyRect :: M.Unbox a => Rect -> (forall s. MVector s a -> Int -> ST s ()) -> Vector a -> Vector a
modifyRect (Rect (Point r1 c1) (Point r2 c2)) modify =
  V.modify $ \v ->
    forM_
      [ fromEnum (Point r c)
        | c <- [c1 .. c2],
          r <- [r1 .. r2]
      ]
      $ modify v

type Lights2 = Vector Int

exec2 :: Instr -> Lights2 -> Lights2
exec2 = \case
  (TurnOn, rect) -> modifyRect rect (`M.modify` (+) 1)
  (Toggle, rect) -> modifyRect rect (`M.modify` (+) 2)
  (TurnOff, rect) -> modifyRect rect (`M.modify` \x -> if x > 0 then x - 1 else 0)

solution :: Solution [Instr] Int Int
solution =
  Solution
    { year = Year 2015,
      day = Day 6,
      S.parseInput = parseInput,
      S.solve1 = \is ->
        let initState = V.replicate 1000000 False
            finalState = foldl' (flip exec) initState is
         in V.foldl' (\z x -> if x then z + 1 else z) 0 finalState,
      S.solve2 = \is ->
        let initState = V.replicate 1000000 0
            finalState = foldl' (flip exec2) initState is
         in V.sum finalState
    }
