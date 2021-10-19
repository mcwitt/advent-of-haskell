{-# LANGUAGE TemplateHaskell #-}

module Main where

import Hedgehog
import Hedgehog.Main
import AOC

prop_test :: Property
prop_test = property $ do
  doAOC === "AOC"

main :: IO ()
main = defaultMain [checkParallel $$(discover)]
