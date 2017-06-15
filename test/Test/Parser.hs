{-# LANGUAGE OverloadedStrings #-}

module Test.Parser where

import Protolude hiding (bool)

import Olifant.Parser
import Olifant.Calculus

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Parser" [literals, symbols, lam, application]

literals :: TestTree
literals = testCase "Literal numbers and booleans" $ do
    expect "42" (Number 42)
    expect "-26" (Number (-26))
    expect "#f" (Bool False)
    expect "#t" (Bool True)

symbols :: TestTree
symbols = testCase "Symbols" $
    expect "hello" (Var "hello")

lam :: TestTree
lam = testCase "λ definitions" $ do
    -- Identity function
    expect "λx.x" (Lam "x" (Var "x"))

    -- K combinator
    expect "λx.λy.x" (Lam "x" (Lam "y" (Var "x")))

    -- S combinator
    expect "λx.λy.λz.x z y z" (Lam "x"
                                (Lam "y"
                                 (Lam "z"
                                   (App (Var "x")
                                    (App (Var "z") (App (Var "y") (Var "z")))))))

    -- B combinator
    expect "λx.λy.λz.x y z" (Lam "x"
                              (Lam "y"
                                (Lam "z" (App (Var "x") (App (Var "y") (Var "z"))))))

application :: TestTree
application = testCase "λ application" $ do
    expect "a b" d
    -- Handle some spaces correctly
    expect " a  b " d
    expect "a   b"  d
  where
    d :: Calculus
    d = App (Var "a") (Var "b")

-- * Helper functions

expect :: Text -> Calculus -> Assertion
expect text expectation = read text @?= Right [expectation]
