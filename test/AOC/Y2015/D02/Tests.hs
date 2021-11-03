{-# LANGUAGE BlockArguments #-}

module AOC.Y2015.D02.Tests (tests) where

import AOC.Solution
import AOC.Y2015.D02
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Day 2" [part1]

part1 :: TestTree
part1 = testCase "part 1" do
  assertEqual "example 1" 101 $ fst $ output solution "2x3x4\n1x1x10"
