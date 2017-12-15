{-# LANGUAGE OverloadedStrings #-}

module Main where

import Protolude
import Test.Tasty

import qualified Test.Compiler as TC
import qualified Test.Gen      as TG
import qualified Test.Parser   as TP

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit Tests" [TC.tests, TG.tests, TP.tests]
