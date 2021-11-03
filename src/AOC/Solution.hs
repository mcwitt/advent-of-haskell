{-# LANGUAGE GADTs #-}

module AOC.Solution where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import Data.List (intercalate)
import Paths_advent_of_haskell (getDataFileName)
import Text.Printf (printf)

newtype Year = Year Int

newtype Day = Day Int

data Solution i o1 o2 where
  Solution ::
    { year :: Year,
      day :: Day,
      parseInput :: ByteString -> i,
      solve1 :: i -> o1,
      solve2 :: i -> o2
    } ->
    Solution i o1 o2

run :: Solution i o1 o2 -> IO (o1, o2)
run s = outputBS s <$> rawInput s

rawInput :: Solution i o1 o2 -> IO ByteString
rawInput s =
  let Year y = year s
      Day d = day s
      path =
        intercalate
          "/"
          [ "data",
            show y,
            printf "%02d" d,
            "input.txt"
          ]
   in getDataFileName path >>= BS.readFile

input :: Solution i o1 o2 -> IO i
input s = parseInput s <$> rawInput s

outputBS :: Solution i o1 o2 -> ByteString -> (o1, o2)
outputBS s = ((,) <$> solve1 s <*> solve2 s) . parseInput s

output :: Solution i o1 o2 -> String -> (o1, o2)
output s = outputBS s . pack
