{-# LANGUAGE OverloadedStrings #-}
module Test.Gen (tests) where

import Protolude

import Olifant.Compiler (compile)
import Olifant.Core
import Olifant.Gen      (llvm)
import Olifant.Parser   (parse)

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "LLVM Code generator" [vars, fn]

vars :: TestTree
vars = testCase "Global variables" $
    c "let x = 42; let y = #t; x" >>= \l -> l @?= Right gen
  where
    gen :: Text
    gen = "; ModuleID = 'calc'\n\n\
          \source_filename = \"<string>\"\n\
          \@x = global i64 42\n\n\
          \@y = global i1 1\n\n\
          \define i64 @main(i64 %_){\n\
          \entry:\n\
          \  ret i64 %x\n}"

fn :: TestTree
fn = testCase "Simple identity function" $
    c "let id = /x:i.x; id 1" >>= \l -> l @?= Right gen
  where
    gen :: Text
    gen = "; ModuleID = 'calc'\n\
          \source_filename = \"<string>\"\n\n\
          \define i64 @id(i64 %x) {\n\
          \entry:\n\
          \  ret i64 %x\n\
          \}\n\n\
          \define i64 @main(i64 %_) {\n\
          \entry:\n\
          \  %0 = call i64 @id(i64 1)\n\
          \  ret i64 %0\n\
          \}\n"

c :: Text -> IO (Either Error Text)
c t = llvm m
  where
    Right (Right m) = compile <$> parse t
