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
    ts = [literals, lam, application, vars, ll]

literals :: TestTree
literals = testCase "Literal numbers, identifiers and booleans" $ do
    expect "42"    [CLit $ Number 42]
    expect "-26"   [CLit $ Number (-26)]
    expect "#f"    [CLit $ Bool False]
    expect "#t"    [CLit $ Bool True]
    expect "hello" [CVar TUnit "hello"]

vars :: TestTree
vars = testCase "Variable definitions" $ do
    expect "a = 1"     [CLet TUnit "a" (CLit $ Number 1)]
    expect "a   =   1" [CLet TUnit "a" (CLit $ Number 1)]
    expect "a=1"       [CLet TUnit "a" (CLit $ Number 1)]
    expect "a:i = b:b" [CLet TInt "a" (CVar TBool "b")]

lam :: TestTree
lam = testCase "λ definitions" $ do
    expect "id x   = x"     [CLam "id" [(TUnit, "x")] [CVar TUnit "x"]]
    expect "id x:b = x"     [CLam "id" [(TBool, "x")] [CVar TUnit "x"]]
    expect "id x:i = x:i"   [CLam "id" [(TInt, "x")] [CVar TInt "x"]]
    expect "id x:i y:i = x" [CLam "id" [(TInt, "x"), (TInt, "y")] [CVar TUnit "x"]]
    expect "f x = x; g x = f x" x
  where
    x = [ CLam "f" [(TUnit, "x")] [CVar TUnit "x"]
        , CLam "g" [(TUnit, "x")] [CApp (CVar TUnit "f") [CVar TUnit "x"]]]

application :: TestTree
application = testCase "λ application" $ do
    expect "a b" d
    expect " a  b " d
    expect "a   b" d
  where
    d = [CApp (CVar TUnit "a") [CVar TUnit "b"]]

ll :: TestTree
ll = testCase "Handle sequences of expressions" $ do
    expect "1; 1"         [CLit $ Number 1, CLit $ Number 1]
    expect "1;1"          [CLit $ Number 1, CLit $ Number 1]
    expect "1 \n 1"       [CLit $ Number 1, CLit $ Number 1]
    expect "id x = x; 1"
      [CLam "id" [(TUnit, "x")] [CVar TUnit "x"], CLit $ Number 1]
    expect "id x = x \n 1"
      [CLam "id" [(TUnit, "x")] [CVar TUnit "x"], CLit $ Number 1]

-- * Helper functions
expect :: Text -> [Calculus] -> Assertion
expect text expectation = parse text @?= Right expectation
