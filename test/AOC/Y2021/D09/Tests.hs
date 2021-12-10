{-# LANGUAGE BlockArguments #-}

module AOC.Y2021.D09.Tests (tests) where

import AOC.Solution (output)
import AOC.Y2021.D09
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Day 9" [example]

example :: TestTree
example = testCase "part 2 examples" do
  let input = "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"
  assertEqual "result" 1134 $ snd $ output solution input
