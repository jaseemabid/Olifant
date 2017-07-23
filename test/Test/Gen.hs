{-# LANGUAGE OverloadedStrings #-}

module Test.Gen
    ( tests
    ) where

import Protolude

import Olifant.Compiler (compile)
import Olifant.Core
import Olifant.Gen      (gen)
import Olifant.Parser   (parse)

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "LLVM Code generator" [vars, fn, shadow]

vars :: TestTree
vars =
    testCase "Global variables" $ do
        ir <- readFile "test/Test/global.ll"
        c "let i = 1; let j = #t; let f = /a:i b:b.42; f i j" >>= \l ->
            l @?= Right ir

fn :: TestTree
fn =
    testCase "Simple identity function" $ do
        ir <- readFile "test/Test/id.ll"
        c "let id = /x:i.x; id 1" >>= \l -> l @?= Right ir

shadow :: TestTree
shadow =
    testCase "Shadow variables" $
    c "let a = 1; let f = /a:i.a; f a" >>= \l -> l @?= Right "~"

c :: Text -> IO (Either Error Text)
c t = gen m
  where
    Right (Right m) = compile <$> parse t
