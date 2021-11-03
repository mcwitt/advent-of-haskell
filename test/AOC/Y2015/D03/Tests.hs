{-# LANGUAGE BlockArguments #-}

module AOC.Y2015.D03.Tests (tests) where

import AOC.Solution
import AOC.Y2015.D03
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Day 3" [examples1, examples2]

examples1 :: TestTree
examples1 = testCase "part 1 examples" do
  assertEqual "example 1" 2 $ fst $ output solution ">"
  assertEqual "example 2" 4 $ fst $ output solution "^>v<"
  assertEqual "example 3" 2 $ fst $ output solution "^v^v^v^v^v"

examples2 :: TestTree
examples2 = testCase "part 2 examples" do
  assertEqual "example 1" 3 $ snd $ output solution "^v"
  assertEqual "example 1" 3 $ snd $ output solution "^>v<"
  assertEqual "example 1" 11 $ snd $ output solution "^v^v^v^v^v"
