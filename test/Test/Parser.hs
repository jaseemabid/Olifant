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
tests = testGroup "Parser" [literals, symbols, lam, combinators, application, lett, ll, v1]

literals :: TestTree
literals = testCase "Literal numbers and booleans" $ do
    expect "42" [Number 42]
    expect "-26" [Number (-26)]
    expect "#f" [Bool False]
    expect "#t" [Bool True]

symbols :: TestTree
symbols = testCase "Symbols" $
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
lett = testCase "Let binding of the form `let a = 1`" $ do
    expect "let a = 1" [Let "a" (Number 1)]
    expect "let a   =   1" [Let "a"(Number 1)]
    expect "let   a = 1" [Let "a" (Number 1)]
    expect "let id = /x.x" [Let "id" (Lam "x" TUnit (Var "x"))]

ll :: TestTree
ll = testCase "Handle sequences of expressions" $ do
    expect "1; 1" [Number 1, Number 1]
    expect "1;1" [Number 1, Number 1]
    expect "1 1" [App (Number 1) (Number 1)]
    expect "1 2 3" [App (Number 1) (App (Number 2) (Number 3))]
    expect "/x.x; 1" [Lam "x" TUnit (Var "x"), Number 1]

v1 :: TestTree
v1 = testCase "Version 1 test program" $
    expect "let id = /x.x; id 42" [Let "id" id, App (Var "id") (Number 42)]
  where
    id = Lam "x" TUnit (Var "x")

-- * Helper functions

expect :: Text -> [Calculus] -> Assertion
expect text expectation = parse text @?= Right expectation
