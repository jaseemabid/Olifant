{-# LANGUAGE OverloadedStrings #-}

module Main where

import LLVM.Pretty (ppllvm)
import Olifant
import Olifant.Core
import Protolude
import Test.Tasty
import Test.Tasty.HUnit

import qualified Test.Gen as TG

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit Tests" [TG.tests]
