{-# LANGUAGE BlockArguments #-}

module AOC.Y2015.D01.Tests (tests) where

import AOC.Solution
import AOC.Y2015.D01
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Day 1" [part1]

part1 :: TestTree
part1 = testCase "part 1" do
  assertEqual "example 1" 0 $ fst $ output solution "(())"
