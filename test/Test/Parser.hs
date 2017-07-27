{-# LANGUAGE OverloadedStrings #-}

-- Known issues
--
-- [todo] - Parser doesn't understand precedence with braces
module Test.Parser (tests) where

import Protolude hiding (bool)

import Olifant.Core
import Olifant.Parser

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Parser" ts
  where
    ts = [literals, lam, application, vars, ll, alias]

literals :: TestTree
literals = testCase "Literal numbers, identifiers and booleans" $ do
    expect "42"    [CLit $ Number 42]
    expect "-26"   [CLit $ Number (-26)]
    expect "#f"    [CLit $ Bool False]
    expect "#t"    [CLit $ Bool True]
    expect "hello" [CVar "hello"]

vars :: TestTree
vars = testCase "Variable definitions" $ do
    expect "a = 1"     [CLet "a" (CLit $ Number 1)]
    expect "a   =   1" [CLet "a" (CLit $ Number 1)]
    expect "a=1"       [CLet "a" (CLit $ Number 1)]

lam :: TestTree
lam = testCase "λ definitions" $ do
    expect "id x   = x"     [CLam "id" [(TUnit, "x")] (CVar "x")]
    expect "id x:b = x"     [CLam "id" [(TBool, "x")] (CVar "x")]
    expect "id x:i = x"     [CLam "id" [(TInt, "x")] (CVar "x")]
    expect "id x:i y:i = x" [CLam "id" [(TInt, "x"), (TInt, "y")] (CVar "x")]

application :: TestTree
application = testCase "λ application" $ do
    expect "a b" d
    expect " a  b " d
    expect "a   b" d
  where
    d = [CApp (CVar "a") [CVar "b"]]

alias :: TestTree
alias = testCase "Let bindings with aliases" $
    expect "let f = id" [CLet "f" (CVar "id")]

ll :: TestTree
ll = testCase "Handle sequences of expressions" $ do
    expect "1; 1"    [a, a]
    expect "1;1"     [a, a]
    expect "1 1"     [CApp a [a]]
    expect "1 2 3"   [CApp a [CLit $ Number 2, CLit $ Number 3]]
    expect "/x.x; 1" [CLam "λ" [x] (CVar "x"), a]
    expect "let id = /x.x; id 1"
      [CLet "id" (CLam "id" [x] (CVar "x")), CApp (CVar "id") [a]]
  where
    a = CLit $ Number 1
    x = (TUnit, "x")

-- * Helper functions
expect :: Text -> [Calculus] -> Assertion
expect text expectation = parse text @?= Right expectation
