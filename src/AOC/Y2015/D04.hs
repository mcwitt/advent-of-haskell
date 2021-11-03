module AOC.Y2015.D04 where

import AOC.Solution (Day (Day), Solution (Solution, day, year), Year (Year))
import qualified AOC.Solution as S
import qualified Crypto.Hash.MD5 as MD5
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as Char8
import Data.List (find)

solve :: Int -> ByteString -> Int
solve numZeros s = let Just n = find p [0 ..] in n
  where
    p n = BS.isPrefixOf (Char8.replicate numZeros '0') $ hash $ show n
    hash salt = Base16.encode $ MD5.hash $ s <> Char8.pack salt

solution :: Solution ByteString Int Int
solution =
  Solution
    { year = Year 2015,
      day = Day 4,
      S.parseInput = Char8.strip,
      S.solve1 = solve 5,
      S.solve2 = solve 6
    }
