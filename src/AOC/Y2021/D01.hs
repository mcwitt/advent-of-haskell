module AOC.Y2021.D01 where

import AOC.Solution (Day (Day), Solution (Solution, day, year), Year (Year))
import qualified AOC.Solution as S
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import Data.Maybe (mapMaybe)

parseInput :: ByteString -> [Int]
parseInput = mapMaybe (fmap fst . Char8.readInt) <$> Char8.lines

solve1 :: [Int] -> Int
solve1 xs = sum $ fromEnum <$> zipWith (<) xs (tail xs)

solve2 :: [Int] -> Int
solve2 xs = sum $ fromEnum <$> zipWith (<) xs (drop 3 xs)

solution :: Solution [Int] Int Int
solution =
  Solution
    { year = Year 2021,
      day = Day 1,
      S.parseInput = parseInput,
      S.solve1 = solve1,
      S.solve2 = solve2
    }
