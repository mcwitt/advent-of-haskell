{-# LANGUAGE LambdaCase #-}

module AOC.Y2021.D03 where

import AOC.Solution (Day (Day), Solution (Solution, day, year), Year (Year))
import qualified AOC.Solution as S
import Control.Arrow ((>>>))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)

type Input = NonEmpty (NonEmpty Bool)

parseInput :: ByteString -> Input
parseInput =
  Char8.lines
    >>> fmap Char8.unpack
    >>> (fmap . fmap)
      ( \case
          '1' -> True
          '0' -> False
          c -> error $ "unexpected character '" <> [c] <> "'"
      )
    >>> fmap NE.fromList
    >>> NE.fromList

solve1 :: Input -> Int
solve1 bss =
  let gammaBin = majority id <$> NE.transpose bss
      epsilonBin = fmap not gammaBin
   in fromBin gammaBin * fromBin epsilonBin
  where
    majority p xs = count p xs >= count (not . p) xs

count :: Traversable t => (a -> Bool) -> t a -> Int
count p = foldr (\x z -> if p x then z + 1 else z) 0

fromBin :: NonEmpty Bool -> Int
fromBin bs =
  sum $
    (\(b, p) -> fromEnum b * 2 ^ p)
      <$> NE.zip (NE.reverse bs) ((0 :: Int) :| [1 ..])

solve2 :: Input -> Int
solve2 bss =
  let o2GenBin = ratingBin (\bs -> count (== True) bs >= count (== False) bs) bss
      co2ScrubberBin = ratingBin (\bs -> count (== True) bs < count (== False) bs) bss
   in fromBin o2GenBin * fromBin co2ScrubberBin

ratingBin :: (NonEmpty Bool -> Bool) -> NonEmpty (NonEmpty Bool) -> NonEmpty Bool
ratingBin f bss = go bss
  where
    go bss = case prune f bss of
      (b, []) -> b :| []
      (b, [bs]) -> b <| bs
      (b, bs' : bss') -> b <| go (bs' :| bss')

prune :: (NonEmpty Bool -> Bool) -> NonEmpty (NonEmpty Bool) -> (Bool, [NonEmpty Bool])
prune f bss =
  let bm = f $ NE.head <$> bss
   in ( bm,
        mapMaybe
          ( \case
              b :| b' : bs' | b == bm -> Just (b' :| bs')
              _ -> Nothing
          )
          $ NE.toList bss
      )

solution :: Solution Input Int Int
solution =
  Solution
    { year = Year 2021,
      day = Day 03,
      S.parseInput = parseInput,
      S.solve1 = solve1,
      S.solve2 = solve2
    }
