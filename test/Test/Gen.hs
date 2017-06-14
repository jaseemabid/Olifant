{-# LANGUAGE OverloadedStrings #-}

module Test.Gen where

import LLVM.Pretty (ppllvm)
import Olifant
import Olifant.Core
import Protolude
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "LLVM Code generator" [assign, fn]

assign :: TestTree
assign = testCase "Global variable assignment" $ pretty [progn] @?= toS gen

  where
    -- | Sample program `x = 1`
    progn :: Core
    progn = Let x one'
      where
        x :: Ref Tipe
        x = Ref "x" TInt

        one' :: Core
        one' = Lit (LNumber 1 TInt)

    gen :: Text
    gen = "; ModuleID = 'calc'\n\n@x = global i64 1"

fn :: TestTree
fn = testCase "Simple identity function" $ pretty [progn] @?= toS gen

  where
    -- | Sample program `x -> x`
    progn :: Core
    progn = Lam id' arg' body'
      where
        id' :: Ref Tipe
        id' = Ref "id" (TInt :~> TInt)

        arg' :: Ref Tipe
        arg' = Ref "x" TInt

        body' :: Expr Tipe
        body' = Var $ Ref "x" TInt

    gen :: Text
    gen = "; ModuleID = 'calc'\n\n\
          \define external ccc i64 @id(i64 %x){\n\
          \entry:\n\
          \  ret i64 %x\n}"
