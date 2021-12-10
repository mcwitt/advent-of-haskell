module Main where

import AOC.Y2015.D01.Tests
import AOC.Y2015.D02.Tests
import AOC.Y2015.D03.Tests
import AOC.Y2015.D04.Tests
import AOC.Y2015.D06.Tests
import AOC.Y2021.D03.Tests
import AOC.Y2021.D09.Tests
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tests"
      [ AOC.Y2015.D01.Tests.tests,
        AOC.Y2015.D02.Tests.tests,
        AOC.Y2015.D03.Tests.tests,
        AOC.Y2015.D04.Tests.tests,
        AOC.Y2015.D06.Tests.tests,
        AOC.Y2021.D03.Tests.tests,
        AOC.Y2021.D09.Tests.tests
      ]
