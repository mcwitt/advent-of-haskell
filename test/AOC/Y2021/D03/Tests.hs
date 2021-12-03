{-# LANGUAGE BlockArguments #-}

module AOC.Y2021.D03.Tests (tests) where

import AOC.Solution (output)
import AOC.Y2021.D03
import Data.ByteString.Char8 (pack)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Day 3" [example2]

exampleInput :: String
exampleInput = "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010"

example2 :: TestTree
example2 = testCase "part 2 examples" do
  assertEqual "example 2a" 23 $ fromBin $ ratingBin (\bs -> count (== True) bs >= count (== False) bs) $ parseInput $ pack exampleInput
  assertEqual "example 2b" 10 $ fromBin $ ratingBin (\bs -> count (== True) bs < count (== False) bs) $ parseInput $ pack exampleInput
  assertEqual "example 2" 230 $ snd $ output solution exampleInput
