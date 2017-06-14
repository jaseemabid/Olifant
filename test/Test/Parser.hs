{-# LANGUAGE OverloadedStrings #-}

module Test.Parser where

import Protolude hiding (bool)
import Text.Parsec

import Olifant.Parser
import Olifant.Calculus

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Parser" [literals, symbols, lam]

literals :: TestTree
literals = testCase "Literal numbers and boolean" $ do
    expect number "42" (Number 42)
    expect number "-26" (Number (-26))
    expect bool "#f" (Bool False)
    expect bool "#t" (Bool True)

symbols :: TestTree
symbols = testCase "Symbols" $
    expect symbol "hello" (Var "hello")

lam :: TestTree
lam = testCase "λ definitions" $ do
    -- Identity function
    expect lambda "λx.x" (Lam "x" (Var "x"))

    -- K combinator
    expect lambda "λx.λy.x" (Lam "x" (Lam "y" (Var "x")))

expect :: Parsec Text () Calculus -> Text -> Calculus -> Assertion
expect grammar text expectation = parse grammar "~" text @?= Right expectation
