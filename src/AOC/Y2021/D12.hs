{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module AOC.Y2021.D12 where

import AOC.Solution
  ( Day (Day),
    Solution (Solution, day, year),
    Year (Year),
  )
import qualified AOC.Solution as S
import qualified Data.Attoparsec.ByteString.Char8 as P
import Data.ByteString (ByteString)
import Data.Char (isLower, isUpper, toLower, toUpper)
import Data.Functor.Base (TreeF (..))
import Data.Functor.Foldable (Recursive (cata))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Tree (Tree (Node))

type Passage = (Cave, Cave)

data Cave = Start | End | Big Label | Small Label deriving (Eq, Ord)

type Label = String

instance Show Cave where
  show Start = "start"
  show End = "end"
  show (Big l) = map toUpper l
  show (Small l) = map toLower l

parseInput :: ByteString -> [Passage]
parseInput s = let Right r = P.parseOnly input s in r
  where
    input = passage `P.sepBy1` P.endOfLine
    passage = (,) <$> cave <* "-" <*> cave
    cave =
      P.choice
        [ Start <$ "start",
          End <$ "end",
          Small <$> P.many1 (P.satisfy isLower),
          Big . fmap toLower <$> P.many1 (P.satisfy isUpper)
        ]

neighbors :: [Passage] -> Cave -> [Cave]
neighbors ps =
  let m =
        Map.fromListWith
          (<>)
          [ p
            | (l, r) <- ps,
              p@(s, _) <- [(l, [r]), (r, [l])],
              s /= End
          ]
   in concat . flip Map.lookup m

generate :: (a -> [a]) -> a -> Tree a
generate g = go
  where
    go x = Node x [go x' | x' <- g x]

paths :: Tree a -> [[a]]
paths = cata go
  where
    go (NodeF x []) = [[x]]
    go (NodeF x ns) = [x : p | ps <- ns, p <- ps]

prune :: Tree Cave -> Tree Cave
prune = go Set.empty
  where
    go visited (Node x ns) = case x of
      Big _ -> Node x [go visited n | n <- ns]
      Start | Set.notMember x visited -> Node x [go (Set.insert x visited) n | n <- ns]
      Small _ | Set.notMember x visited -> Node x [go (Set.insert x visited) n | n <- ns]
      _ -> Node x []

prune2 :: Tree Cave -> Tree Cave
prune2 = go Set.empty True
  where
    go visited canRevisit (Node x ns) = case x of
      Big _ -> Node x [go visited canRevisit n | n <- ns]
      Start | Set.notMember x visited -> Node x [go (Set.insert x visited) canRevisit n | n <- ns]
      Small _ | Set.notMember x visited -> Node x [go (Set.insert x visited) canRevisit n | n <- ns]
      Small _ | canRevisit -> Node x [go visited False n | n <- ns]
      _ -> Node x []

solve1 :: [Passage] -> Int
solve1 ps =
  length
    . filter ((== End) . last)
    . paths
    . prune
    . generate (neighbors ps)
    $ Start

solve2 :: [Passage] -> Int
solve2 ps =
  length
    . filter ((== End) . last)
    . paths
    . prune2
    . generate (neighbors ps)
    $ Start

solution :: Solution [Passage] Int Int
solution =
  Solution
    { year = Year 2021,
      day = Day 12,
      S.parseInput = parseInput,
      S.solve1 = solve1,
      S.solve2 = solve2
    }
