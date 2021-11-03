{-# LANGUAGE BlockArguments #-}

module AOC.Y2015.D04.Tests (tests) where

import AOC.Solution
import AOC.Y2015.D04
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Day 4" [part1]

part1 :: TestTree
part1 = testCase "part 1" do
  assertEqual "example 1" 609043 $ fst $ output solution "abcdef"
  assertEqual "example 2" 1048970 $ fst $ output solution "pqrstuv"
