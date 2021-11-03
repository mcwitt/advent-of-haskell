{-# LANGUAGE BlockArguments #-}

module AOC.Y2015.D06.Tests (tests) where

import AOC.Solution
import AOC.Y2015.D06
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Day 6" [part1, part2]

part1 :: TestTree
part1 = testCase "part 1" do
  assertEqual "start with all off" 0 $ fst $ output solution ""
  assertEqual "example 1" 1000000 $ fst $ output solution "turn on 0,0 through 999,999"
  assertEqual "example 2" 1000 $ fst $ output solution "toggle 0,0 through 999,0"
  assertEqual "example 3" 999996 $ fst $ output solution "turn on 0,0 through 999,999\nturn off 499,499 through 500,500"

part2 :: TestTree
part2 = testCase "part 2" do
  assertEqual "example 1" 1 $ snd $ output solution "turn on 0,0 through 0,0"
  assertEqual "example 2" 2000000 $ snd $ output solution "toggle 0,0 through 999,999"
