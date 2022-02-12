{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module AOC.Y2021.D11.Tests (tests) where

import AOC.Y2021.D11
import Test.Tasty
import Test.Tasty.HUnit
import Text.RawString.QQ

tests :: TestTree
tests = testGroup "Day 11" [example]

example :: TestTree
example = testCase "examples" do
  let input =
        parseInput
          [r|5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526|]

      results =
        input :
        fmap
          parseInput
          [ [r|6594254334
3856965822
6375667284
7252447257
7468496589
5278635756
3287952832
7993992245
5957959665
6394862637|],
            [r|8807476555
5089087054
8597889608
8485769600
8700908800
6600088989
6800005943
0000007456
9000000876
8700006848|],
            [r|0050900866
8500800575
9900000039
9700000041
9935080063
7712300000
7911250009
2211130000
0421125000
0021119000|]
          ]

  assertEqual "results" results $ take 4 $ iterate (fst . update) input
