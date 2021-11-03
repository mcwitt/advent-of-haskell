module AOC.Y2015.D02 where

import AOC.Solution (Day (Day), Solution (Solution, day, year), Year (Year))
import qualified AOC.Solution as S
import Data.Attoparsec.ByteString (parseOnly, sepBy)
import Data.Attoparsec.ByteString.Char8 (char, decimal, endOfLine)
import Data.ByteString.Char8 (ByteString)
import Data.List (sort)

data Box = Box Int Int Int deriving (Show)

parseInput :: ByteString -> [Box]
parseInput s = let Right r = parseOnly input s in r
  where
    input = box `sepBy` endOfLine
    box =
      Box
        <$> decimal <* char 'x'
        <*> decimal <* char 'x'
        <*> decimal

paper :: Box -> Int
paper (Box l w h) =
  let areas = [l * w, l * h, w * h]
      total = 2 * sum areas
      smallest = minimum areas
   in total + smallest

solve1 :: [Box] -> Int
solve1 = sum . fmap paper

ribbon :: Box -> Int
ribbon (Box l w h) =
  let [x, y] = take 2 $ sort [l, w, h]
      wrap = 2 * (x + y)
      bow = l * w * h
   in wrap + bow

solve2 :: [Box] -> Int
solve2 = sum . fmap ribbon

solution :: Solution [Box] Int Int
solution =
  Solution
    { year = Year 2015,
      day = Day 2,
      S.parseInput = parseInput,
      S.solve1 = solve1,
      S.solve2 = solve2
    }
