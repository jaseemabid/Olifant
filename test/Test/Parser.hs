{-# LANGUAGE OverloadedStrings #-}

module Test.Parser where

import Protolude
import Text.Parsec

import Olifant
import Olifant.Core
import Olifant.Parser
import Olifant.Calculus

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Parser" [num]

num :: TestTree
num = testCase "Numbers" $ do
    parse number "~" "42" @?= Right (Number 42)
    parse number "~"  "-26" @?= Right (Number (-26))
