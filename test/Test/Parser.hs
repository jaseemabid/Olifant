{-# LANGUAGE OverloadedStrings #-}

-- Known issues
--
-- [todo] - Parser doesn't understand precedence with braces

module Test.Parser (tests) where

import Protolude hiding (bool)

import Olifant.Calculus
import Olifant.Core     (Tipe (..))
import Olifant.Parser

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Parser" [literals, lam, combinators, application,
                            lett, ll, alias]

literals :: TestTree
literals = testCase "Literal numbers, identifiers and booleans" $ do
    expect "42" [Number 42]
    expect "-26" [Number (-26)]
    expect "#f" [Bool False]
    expect "#t" [Bool True]
    expect "hello" [Var "hello"]

lam :: TestTree
lam = testCase "λ definitions" $ do
    -- Identity function
    expect "λx.x"   [Lam "x" TUnit (Var "x")]
    expect "λx:b.x" [Lam "x" TBool (Var "x")]
    expect "λx:i.x" [Lam "x" TInt  (Var "x")]

combinators :: TestTree
combinators = testCase "Combinators" $ do
    -- K combinator
    expect "λx.λy.x" [Lam "x" TUnit (Lam "y" TUnit (Var "x"))]

    -- S combinator
    expect "λx.λy.λz.x z y z" [Lam "x" TUnit
                                (Lam "y" TUnit
                                 (Lam "z" TUnit
                                   (App (Var "x")
                                    (App (Var "z") (App (Var "y") (Var "z"))))))]

    -- B combinator
    expect "λx.λy.λz.x y z" [Lam "x" TUnit
                              (Lam "y" TUnit
                                (Lam "z" TUnit (App (Var "x") (App (Var "y") (Var "z")))))]

application :: TestTree
application = testCase "λ application" $ do
    expect "a b" d
    -- Handle some spaces correctly
    expect " a  b " d
    expect "a   b"  d
  where
    d = [App (Var "a") (Var "b")]

lett :: TestTree
lett = testCase "Let expressions" $ do
    expect "let a = 1" [Let "a" (Number 1)]
    expect "let a   =   1" [Let "a"(Number 1)]
    expect "let   a = 1" [Let "a" (Number 1)]
    expect "let id = /x.x" [Let "id" (Lam "x" TUnit (Var "x"))]

alias :: TestTree
alias = testCase "Let bindings with aliases" $
    expect "let f = id" [Let "f" (Var "id")]

ll :: TestTree
ll = testCase "Handle sequences of expressions" $ do
    expect "1; 1" [a, a]
    expect "1;1" [a, a]
    expect "1 1" [App a a]
    expect "1 2 3" [App a (App (Number 2) (Number 3))]
    expect "/x.x; 1" [Lam "x" TUnit (Var "x"), a]
    expect "let id = /x.x; id 1" [Let "id"  (Lam "x" TUnit (Var "x")), App (Var "id") a]
  where
    a = Number 1

-- * Helper functions

expect :: Text -> [Calculus] -> Assertion
expect text expectation = parse text @?= Right expectation
