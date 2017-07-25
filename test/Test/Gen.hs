{-# LANGUAGE OverloadedStrings #-}

module Test.Gen (tests) where

import Protolude

import Olifant.Compiler (compile)
import Olifant.Gen      (gen)
import Olifant.Parser   (parse)

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "LLVM Code generator" [t1, t2, t3, t4]

t1 :: TestTree
t1 = testCase "Identity function" $ do
    ir <- readFile "test/ll/id.ll"
    t "let id = /x:i.x; id 42" ir

t2 :: TestTree
t2 = testCase "Const function" $ do
    ir <- readFile "test/ll/const.ll"
    t "let c = /x:i.1; c 42" ir

t3 :: TestTree
t3 = testCase "Global variables" $ do
    ir <- readFile "test/ll/global.ll"
    t "let i = 1; let j = #t; let f = /a:i b:b.42; f i j" ir

t4 :: TestTree
t4 = testCase "Shadow variables" $ do
    ir <- readFile "test/ll/shadow.ll"
    t "let a = 1; let f = /a:i.a; f a" ir

t :: Text -> Text -> IO ()
t code ir = case compile <$> parse code of
    Right (Right x) -> gen x >>= \l -> l @?= Right ir
    err -> assertFailure $ show err
